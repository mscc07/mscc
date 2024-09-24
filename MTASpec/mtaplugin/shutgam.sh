#!/bin/sh
cp $1-ports ~/jobs/
python ~/mtaplugin/gamserver/shutdowngamserver.py $1-
