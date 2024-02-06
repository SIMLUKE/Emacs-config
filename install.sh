#!/bin/bash

read -p "This installation will place your old config inside a old folder do you wish to continue ? (y/n): " pushChoice
if [ "$pushChoice" = "y" ]; then
    rm -rf ~/old_emacs
    mkdir ~/old_emacs
    mv ~/.emacs* ~/old_emacs
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

echo "This config needs clangd to work, installing."
PS3="Please select your OS : "
options=("Arch" "Fedora")
select commitPrefix in "${options[@]}"; do
    case $commitPrefix in
        "Fedora")
            sudo dnf install clang-tools-extra
            sudo dnf install pyright
	    break
            ;;
        "Arch")
            sudo pacman -S pyright
            sudo pacman -S clang
	    break
	    ;;
        *)
            echo "Invalid choice. Please select a valid option."
            ;;
    esac
done

echo "Adding the aliases to your dot config."
PS3="Please select your terminal interpretor : "
options=("BASH" "ZSH")
select commitPrefix in "${options[@]}"; do
    case $commitPrefix in
        "ZSH")
            echo "alias ne='emacsclient -nw -s tiny'" >> ~/.zshrc
            echo "alias VSemacs='emacsclient -nw'" >> ~/.zshrc
            source ~/.zshrc
	    break
            ;;
        "BASH")
            echo "alias ne=\"emacsclient -nw -s tiny\"" >> ~/.bashrc
            echo "alias VSemacs=\"emacsclient -nw\"" >> ~/.bashrc
            source ~/.bashrc
            break
	    ;;
        *)
            echo "Invalid choice. Please select a valid option."
            ;;
    esac
done

echo "Installation done, starting emacs with restart_emacs.sh script :"
./restart_emacs.sh
echo "I hope you read the README.md or else this is going to be a bad install for you ..."
