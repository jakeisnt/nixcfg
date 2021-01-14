#!/usr/bin/env zsh

alias e='emacsclient -c'
ediff() { e --eval "(ediff-files \"$1\" \"$2\")"; }
