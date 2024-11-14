#!/bin/bash

PS3='Please enter your choice: '
options=("Install config" "Restart emacs" "Update emacs" "Generate config file" "Restore old config" "Quit")
select opt in "${options[@]}"; do
    case $opt in
    "Install config")
        ./scripts/install.sh
        break
        ;;
    "Restart emacs")
        ./scripts/restart_emacs.sh
        break
        ;;
    "Update emacs")
        ./scripts/update.sh
        break
        ;;
    "Restore old config")
        ./scripts/restore_old_emacs.sh
        break
        ;;
    "Generate config file")
        ./scripts/conf_gen.sh
        break
        ;;
    "Quit")
        break
        ;;
    *) echo "invalid option $REPLY" ;;
    esac
done
