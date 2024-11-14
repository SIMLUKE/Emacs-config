#!/bin/bash

pgrep_output=$(pgrep emacs)
pgrep_arr=($pgrep_output)
if [[ "${#pgrep_arr[@]}" == "1" ]] || [[ "${#pgrep_arr[@]}" == "0" ]]; then
    echo "emacs is not running, starting ..."
    yes | emacs --with-profile tiny --daemon=tiny
    yes | emacs --daemon
else
    echo "emacs is running, restarting ..."
    killall emacs
    yes | emacs --with-profile tiny --daemon=tiny
    yes | emacs --daemon
fi
