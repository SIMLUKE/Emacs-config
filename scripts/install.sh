#!/bin/bash

source ./scripts/funcs.sh

./scripts/conf_gen.sh

read -p "This installation will place your old config inside a old folder do you wish to continue ? (y/n): " pushChoice
if [ "$pushChoice" = "y" ]; then
    if [ ! -d $HOME/old_emacs ]; then
        mkdir $HOME/old_emacs
    fi
    if test -n "$(find $HOME -maxdepth 1 -name '.emacs*' -print -quit)"; then
        latestconfig=$(ls $HOME/old_emacs | grep config | sed -n 's/^config//p' | sort -n | tail -1)
        newconfig=$((latestconfig + 1))

        new_old_dir="$HOME/old_emacs/config$newconfig"
        mkdir $new_old_dir

        mv $HOME/.emacs* $new_old_dir
    fi
else
    echo "Aborted."
    exit
fi

echo "cloning chemacs for multiple emacs configs."
git clone https://github.com/plexus/chemacs2.git ~/.emacs.d

echo "Setting up folders"
cp .emacs-profiles.el ~/
cp -r .emacs.default/ ~/
cp -r .emacs.tiny/ ~/

install_dependencies

echo "Adding the aliases to your dot config."
PS3="Please select your terminal interpretor : "
options=("BASH" "ZSH" "SKIP")
select commitPrefix in "${options[@]}"; do
    case $commitPrefix in
    "ZSH")
        echo "alias emacs='emacs -Q'" >>~/.zshrc
        echo "alias ne='emacsclient -nw -s tiny'" >>~/.zshrc
        echo "alias VSemacs='emacsclient -nw'" >>~/.zshrc
        echo "alias nee='emacsclient -c -s tiny'" >>~/.zshrc
        echo "alias VSgui='emacsclient -c'" >>~/.zshrc
        source ~/.zshrc
        break
        ;;
    "BASH")
        echo "alias emacs=\"emacs -Q\"" >>~/.bashrc
        echo "alias ne=\"emacsclient -nw -s tiny\"" >>~/.bashrc
        echo "alias VSemacs=\"emacsclient -nw\"" >>~/.bashrc
        echo "alias nee=\"emacsclient -c -s tiny\"" >>~/.bashrc
        echo "alias VSgui=\"emacsclient -c\"" >>~/.bashrc
        source ~/.bashrc
        break
        ;;
    "SKIP")
        echo "Skiped"
        break
        ;;
    *)
        echo "Invalid choice. Please select a valid option."
        ;;
    esac
done

echo "Installation done, starting emacs with restart_emacs.sh script :"
./scripts/restart_emacs.sh
echo "I hope you read the README.md or else this is going to be a bad install for you ..."
