#!/bin/bash

echo "Uptating ..."
git pull

echo "Updtading dependencies..."
PS3="Please select your OS : "
options=("Arch" "Fedora" "Debian" "SKIP")
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
        "Debian")
            sudo apt install clang
            echo "/!\ You won't have pyright with this pakage manager!"
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

cp .emacs.default/init.el ~/.emacs.default/
cp .emacs.tiny/init.el ~/.emacs.tiny/

cp .emacs.default/custom/* ~/.emacs.default/custom/
cp .emacs.tiny/custom/* ~/.emacs.tiny/custom/

cp .emacs.default/epitech/* ~/.emacs.default/epitech/
cp .emacs.tiny/epitech/* ~/.emacs.tiny/epitech/

cp .emacs.default/languages/* ~/.emacs.default/languages/

./restart_emacs.sh
