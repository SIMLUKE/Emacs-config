#!/bin/bash

echo "Restoring old emacs ..."
if [ ! -d $HOME/old_emacs ]; then
    mkdir $HOME/old_emacs
fi

if [ -z "$(ls -A "$HOME/old_emacs")" ]; then
    newconfig="1"

    new_old_dir="$HOME/old_emacs/config$newconfig"
    mkdir $new_old_dir
    mv $HOME/.emacs* $new_old_dir
else
    configs=($HOME/old_emacs/*)
    PS3="Select the config you wish to restore: "
    select torestore in "${configs[@]}"; do

        if test -n "$(find $HOME -maxdepth 1 -name '.emacs*' -print -quit)"; then
            latestconfig=$(ls $HOME/old_emacs | grep config | sed -n 's/^config//p' | sort -n | tail -1)
            newconfig=$((latestconfig + 1))

            new_old_dir="$HOME/old_emacs/config$newconfig"
            mkdir $new_old_dir

            mv $HOME/.emacs* $new_old_dir
        fi

        mv $torestore/.emacs* $HOME
        rmdir $torestore
        break
    done
fi
