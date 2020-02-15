#!/bin/bash

USER=xxxx
API_KEY=12345

cd /home/$USER/package.elm-lang.org

./dist/build/run-server/run-server --github=$API_KEY --port=8019 &

echo $! > monit/package.pid
