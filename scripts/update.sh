#!/bin/bash

echo "Uptating ..."
git pull

source ./scripts/funcs.sh

echo "Updtading dependencies..."

install_dependencies

cp .emacs.default/init.el ~/.emacs.default/
cp .emacs.tiny/init.el ~/.emacs.tiny/

cp -r .emacs.default/custom/* ~/.emacs.default/custom/
cp -r .emacs.tiny/custom/* ~/.emacs.tiny/custom/

cp .emacs.default/epitech/* ~/.emacs.default/epitech/
cp .emacs.tiny/epitech/* ~/.emacs.tiny/epitech/

cp .emacs.default/languages/* ~/.emacs.default/languages/

./scripts/restart_emacs.sh
