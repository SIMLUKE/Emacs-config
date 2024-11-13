#!/bin/bash

if [ $# -ne 0 ]; then
    if [ ! -d $HOME/old_emacs/config$1 ]; then
        echo "Error : $HOME/old_emacs/config$1 does not exist"
        exit 1
    fi
    echo "Restoring config$1"
    mv $HOME/old_emacs/config$1/.emacs* $HOME
    rmdir "$HOME/old_emacs/config$1"
    exit 0
fi
latestconfig=$(ls $HOME/old_emacs | grep config | sed -n 's/^config//p' | sort -n | tail -1)
newconfig=$((latestconfig + 1))

new_old_dir="$HOME/old_emacs/config$newconfig"

echo "Restoring old emacs ..."
if [ ! -d $new_old_dir ]; then
    mkdir $new_old_dir
fi

mv $HOME/.emacs* $new_old_dir
mv $HOME/old_emacs/.emacs* $HOME
echo "Done."
