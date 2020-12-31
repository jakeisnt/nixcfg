#!/usr/bin/env zsh

alias e='emacsclient -n'
alias cat='bat' # bat is just better
ediff() { e --eval "(ediff-files \"$1\" \"$2\")"; }
