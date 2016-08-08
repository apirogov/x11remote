#!/usr/bin/env python
from optparse import OptionParser
from shutil import which
from subprocess import check_output, CalledProcessError
from sys import stderr

from flask import Flask, jsonify
from flask_sockets import Sockets

def get_parser():
    parser = OptionParser(usage='%prog [-p|--port PORT] [-w|--websockets]')
    parser.add_option('-p', '--port',
            help='Port for X11Remote to listen on (default: %default)')
    parser.add_option('-w', '--websockets', action='store_true',
            help='Enable websocket support')
    parser.set_defaults(port=1234)
    return parser

def missing_tool_exit(*args):
    for tool in args:
        if not which(tool):
            print(tool+' not found! Please add a '+tool+' binary to your PATH!',
                file=sys.stderr)
            exit(1)

def xdotool(*args):
    args = list(args)
    if args[0]=='mousemove_relative':
        args.insert(1, '--')
    args.insert(0, 'xdotool')
    try:
        return check_output(args).decode('utf-8')
    except CalledProcessError as e:
        return e.output.decode('utf-8')

app = Flask(__name__, static_url_path='')
sockets = Sockets(app)

@sockets.route('/')
def handle_socket(ws):
    while not ws.closed:
        print(ws.receive())

@app.route("/keymap.json")
def get_xmodmap():
    dic = {}
    lines = str(check_output(['xmodmap','-pke'])).split('\\n')
    for l in lines:
        ls = l.split('=')
        if len(ls) != 2 or len(ls[1]) < 1:
            continue
        dic[ls[0].split()[1]] = ls[1].split()
    return jsonify(dic), {'Content-Type': 'text/json; charset=utf-8'}

@app.route("/exec/<string:cmds>")
def exec(cmds):
    for cmd in cmds.split('|'):
        xdotool(*cmd.split())
    return "ACK"

@app.route("/")
def serve():
    return app.send_static_file('index.html')

if __name__ == '__main__':
    opts,args = get_parser().parse_args()
    missing_tool_exit('xdotool','xmodmap')

    from gevent.pywsgi import WSGIServer, WSGIHandler
    if opts.websockets:
        from geventwebsocket.handler import WebSocketHandler
    http_server = WSGIServer(('', opts.port), app,
            handler_class=WebSocketHandler if opts.websockets else WSGIHandler)
    http_server.serve_forever()
