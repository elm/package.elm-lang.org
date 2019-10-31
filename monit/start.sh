#!/bin/bash

USER=xxxx
PATH=/home/$USER/compiler/dist/build/elm/:$PATH
API_KEY=12345

cd /home/$USER/websites/package/

./dist/build/run-server/run-server --github=$API_KEY --port=8019 &

echo $! > monit/package.pid
