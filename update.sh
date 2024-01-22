#!/bin/bash

echo "Uptating ..."
git pull

cp .emacs.default/init.el ~/.emacs.default/
cp .emacs.tiny/init.el ~/.emacs.tiny/

cp .emacs.default/custom/* ~/.emacs.default/custom/
cp .emacs.tiny/custom/* ~/.emacs.tiny/custom/

cp .emacs.default/epitech/* ~/.emacs.default/epitech/
cp .emacs.tiny/epitech/* ~/.emacs.tiny/epitech/

./restart_emacs.sh
