#!/usr/bin/env python3

"""Serve the given ZIP archive over HTTP on localhost."""

import http.server
import socketserver
import sys
import tempfile
import zipfile

with tempfile.TemporaryDirectory() as root:
    with zipfile.ZipFile(sys.argv[1], "r") as archive:
        archive.extractall(root)

    class Handler(http.server.SimpleHTTPRequestHandler):
        def __init__(self, *args, **kwargs):
            super(Handler, self).__init__(*args, directory=root, **kwargs)

    with socketserver.TCPServer(("127.0.0.1", 0), Handler) as httpd:
        print("http://%s:%s" % httpd.server_address)
        httpd.serve_forever()
