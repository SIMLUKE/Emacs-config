# Emacs Config
>[!CAUTION]
>Please read this page carefully, you need to change your way of using emacs in oder to maximize the potential of this config.

## Install
Just execute my script bro (will take some time)

## Emacs Daemons
### Quick explenation
This config was made in order to run on daemons, this allows emacs to be instantly opened once the daemon is started, doing otherwise would be stupid.

### Usage
1. **BEFORE EVERYTHING** run the provided "restart_emacs.sh" script, this will start or restart the daemons, i would suggest adding this to your autostart on your computer (the install script will run it for you the first time as he need to load all the packages)
2. VS CODE like editor, this config runs on the "emacs" daemon, in order to start an instance use the command : `emacsclient -c`
3. Tiny instance, this config runs on the "emacs tiny" daemon, starting an instance like the previous would be stupid, you could use it like this `emacsclient -c -s tiny "file"`

>[!TIP]
>I would **HIGHLY** suggest you add this code to your ".bashrc" config :
>```
>alias ne="emacsclient -nw -s tiny"
>alias VSemacs="emacsclient -nw"
>```
>Or in a .zshrc config
>```
>alias ne "emacsclient -nw -s tiny"
>alias VSemacs "emacsclient -nw"
>```
>Use ne _file_ to open a file, and VSemacs to start the vscode like emacs
>(for user with emacs gui only, DM me for config)

## Keybings
If you are unfamiliar with emacs's base keybindings i would suggest reading this : [emacs cheatsheet](https://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf). (please use ctr-y, ctr-w and alt-w, for paste, cut, copy at least)
As for the config, it contains custom keybinds inside the [keybinds file](custom/keyboard-shortcuts.el)
The most usefull are :
1. `ctr-t` : opens file browser
2. `ctr-c s` : change the file browser directory
3. `ctr-c r` : replace all string by another
4. `ctr-c v` : allows copy in the vterm window
5. `ctr-n` : opens a new tab (if you want the vs code tab experience)
6. `ctr-q` : close said tab
7. `ctr-x t f` : looks for a file in the file browser
8. `ctr-x t b` : bookmarks a file
9. `ctr-x g` : git status of project
