module Main where
import Data.Monoid
import qualified Data.Map as M
import Data.Maybe (mapMaybe,fromMaybe)
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans (liftIO,MonadIO)

import Data.String (fromString)
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (Text, fromStrict, unpack)

import System.Process
import System.Exit
import System.IO (hPutStrLn, stderr)
import System.IO.Error (catchIOError, isDoesNotExistError)

import Options.Applicative
import Data.FileEmbed
import Web.Scotty
import Network.WebSockets
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))

data Args = Args { argPort :: Int, argVerbose :: Bool
                 , argWebsockets :: Bool, argDebug :: Bool }

parseArgs :: Parser Args
parseArgs = Args
        <$> option auto (long "port" <> short 'p' <> metavar "PORT"
         <> help "Port for X11Remote to listen on" <> value 1234 <> showDefault)
        <*> switch (long "verbose" <> short 'v' <> help "Enable verbose server log")
        <*> switch (long "websockets" <> short 'w' <> help "Enable websocket support")
        <*> switch (long "debug" <> short 'd' <> help "Debug mode (serve files from ./static/)")

main = do
  args <- execParser $ info (helper <*> parseArgs) fullDesc
  missingToolExit "xdotool"
  missingToolExit "xmodmap"
  if argWebsockets args then do
    httpApp <- scottyApp $ myScottyApp args
    run (argPort args) $ websocketsOr
                           defaultConnectionOptions (wsApp $ argVerbose args)
                           httpApp
  else scotty (argPort args) $ myScottyApp args

-- check that a program with given name exists/can be called
toolExists str = (createProcess (proc str [])
                    {std_out=CreatePipe, std_err=CreatePipe} >> return True)
                    `catchIOError`
                    (\e -> if isDoesNotExistError e then return False else return True)

missingToolExit str = toolExists str >>= \exists ->
  unless exists $ do
    hPutStrLn stderr $ str++" not found! Please add a "++str++" binary to your PATH!"
    exitFailure

-- websocket server listening for xdotool commands (similar to GET API)
wsApp :: Bool -> ServerApp
wsApp verbose pending = do
    connection <- acceptRequest pending
    when verbose $ putStrLn "Websocket connection accepted"
    forkPingThread connection 30
    forever $ do
        message <- unpack <$> receiveData connection
        when verbose $ putStrLn $ "Received: "++message
        mapM_ (xdotool . words) $ splitOn "|" message
        sendTextData connection $ decodeUtf8 $ fromString "ACK"

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

  -- receive xdotool commands, one or many, seperated by |
  get "/exec/:cmds" $ do
    tmp <- param "cmds"
    let cmds = map words $ splitOn "|" tmp
    mapM_ xdotool cmds
    text "ACK"

xdotool :: (MonadIO m) => [String] -> m Text
xdotool args = do
 let args' = if head args == "mousemove_relative"
             then head args:"--":tail args
             else args
 ret <- liftIO $ readProcess "xdotool" args' "" `catchIOError` (\e -> return $ show e)
 return $ fromString ret

-- get the current xmodmap settings as map of (keycode,[different keysyms])
getXModmap = parseKeymap <$> liftIO (readProcess "xmodmap" ["-pke"] "")
  where parseKeymap = M.fromList . mapMaybe (toEntry . words) . lines
        toEntry s = if length l/=2 || length r<1 then Nothing
                    else Just (last l, tail r)
          where (l,r) = break (=="=") s

embeddedStatic :: [(FilePath, BS.ByteString)]
embeddedStatic = $(embedDir "static")
serveStatic str
 | "html" `isSuffixOf` str = html txt
 | otherwise = text txt
 where txt = fromStrict $ decodeUtf8 $ fromMaybe BS.empty $ lookup str embeddedStatic

