module Main where
import Data.Monoid
import qualified Data.Map as M
import Data.Maybe (mapMaybe,fromMaybe)
import Data.List (isSuffixOf)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans (liftIO)

import Data.String (fromString)
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (fromStrict)

import System.Process
import System.Exit
import System.IO (hPutStrLn, stderr)
import System.IO.Error (catchIOError, isDoesNotExistError)

import Options.Applicative
import Data.FileEmbed
import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))

data Args = Args { argPort :: Int, argVerbose :: Bool, argDebug :: Bool }

parseArgs :: Parser Args
parseArgs = Args
        <$> option auto (long "port" <> short 'p' <> metavar "PORT"
         <> help "Port for X11Remote to listen on" <> value 1234 <> showDefault)
        <*> switch (long "verbose" <> short 'v' <> help "Enable verbose server log")
        <*> switch (long "debug" <> short 'd' <> help "Debug mode (serve files from ./static/)")

main = do
  args <- execParser $ info (helper <*> parseArgs) fullDesc
  missingToolExit "xdotool"
  missingToolExit "xmodmap"
  scotty (argPort args) $ myScottyApp args

-- check that a program with given name exists/can be called
toolExists str = (createProcess (proc str [])
                    {std_out=CreatePipe, std_err=CreatePipe} >> return True)
                    `catchIOError`
                    (\e -> if isDoesNotExistError e then return False else return True)

missingToolExit str = toolExists str >>= \exists ->
  unless exists $ do
    hPutStrLn stderr $ str++" not found! Please add a "++str++" binary to your PATH!"
    exitFailure

myScottyApp args = do
  when (argVerbose args) $ middleware logStdoutDev
  if argDebug args
  then do --serve additional files from static directory
    get "/" $ file "static/index.html"
    middleware $ staticPolicy (noDots >-> addBase "static") -- for pics, JS stuff
  else do --serve additional files from embedded data in binary (-> no deps)
    get "/" $ serveStatic "index.html"
    mapM_ ((\s -> get (fromString $ "/"++s) $ serveStatic s) . fst) embeddedStatic

  -- return xmodmap keymap (get always fresh)
  get "/keymap.json" $ getXModmap >>= json

  -- xdotool API
  get "/key/:str" $ xkeycmd "key" "str"
  get "/keydown/:str" $ xkeycmd "keydown" "str"
  get "/keyup/:str" $ xkeycmd "keyup" "str"

  get "/mousemove/:x/:y" $ xmovecmd "mousemove" "x" "y" False
  get "/mousemove_relative/:x/:y" $ xmovecmd "mousemove_relative" "x" "y" True

  get "/click/:btn" $ xbtncmd "click" "btn"
  get "/mousedown/:btn" $ xbtncmd "mousedown" "btn"
  get "/mouseup/:btn" $ xbtncmd "mouseup" "btn"

embeddedStatic :: [(FilePath, BS.ByteString)]
embeddedStatic = $(embedDir "static")
serveStatic str
 | "html" `isSuffixOf` str = html txt
 | otherwise = text txt
 where txt = fromStrict $ decodeUtf8 $ fromMaybe BS.empty $ lookup str embeddedStatic

-- helper actions: execute xdotool with given arguments, render result
xkeycmd cmd keyp = param keyp >>= \k -> xdotool [cmd,k]
xbtncmd cmd btnp = param btnp >>= \b -> xdotool [cmd,show $ b+(0::Int)]
xmovecmd cmd xp yp rel = do
  x <- show . (+(0::Int)) <$>  param xp
  y <- show . (+(0::Int)) <$> param yp
  xdotool $ if rel then [cmd,"--",x,y] else [cmd,x,y]

xdotool args = do
 ret <- liftIO (readProcess "xdotool" args "")
 text $ fromString ret

-- get the current xmodmap settings as map of (keycode,[different keysyms])
getXModmap = parseKeymap <$> liftIO (readProcess "xmodmap" ["-pke"] "")
  where parseKeymap = M.fromList . mapMaybe (toEntry . words) . lines
        toEntry s = if length l/=2 || length r<1 then Nothing
                    else Just (last l, tail r)
          where (l,r) = break (=="=") s

