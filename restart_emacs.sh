#!/bin/bash

pgrep_output=$(pgrep emacs)
pgrep_arr=($pgrep_output)
if [[ "${#pgrep_arr[@]}" == "1" ]] || [[ "${#pgrep_arr[@]}" == "0" ]]; then
    echo "emacs is not running. Starting emacs."
    emacs --with-profile tiny --daemon=tiny
    emacs --daemon
else
    echo "emacs is running. restarting emacs."
    killall emacs
    emacs --with-profile tiny --daemon=tiny
    emacs --daemon
fi
