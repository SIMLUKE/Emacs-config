# Emacs Config
>[!CAUTION]
>Please read this page carefully, you need to change your way of using emacs in oder to maximize the potential of this config.

## Install
Just execute my script bro (will take some time)

(if asked project root, awnser ".")

(if asked to compile vterm say yes)


## Update
Just execute my script bro (will take some time)

## Emacs Daemons
### Quick explenation
This config was made in order to run on daemons, this allows emacs to be instantly opened once the daemon is started, doing otherwise would be stupid.

### Usage
1. **BEFORE EVERYTHING** run the provided "restart_emacs.sh" script, this will start or restart the daemons, i would suggest adding this to your autostart on your computer (the install script will run it for you the first time as he need to load all the packages but this script will need to be run each time you reeboot your computer)
2. VS CODE like editor, this config runs on the "emacs" daemon, in order to start an instance use the command : `VSemacs`
3. Tiny instance, this config runs on the "emacs tiny" daemon, starting an instance like the previous would be stupid, you could use it like this `ne "file_name"`

>[!IMPORTANT]
>The LSP mode (the thing that shows you warnings) runs off clang (that's why you install it) to modifie the flags it compiles with (add more warnings or a -I./include) copy the the (compile_flags.txt) in the root of your repository


## Keybings
If you are unfamiliar with emacs's base keybindings i would suggest reading this : [emacs cheatsheet](https://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf). (please use ctr-y, ctr-w and alt-w, for paste, cut, copy at least)
As for the config, it contains custom keybinds inside the [keybinds file](.emacs.default/custom/keyboard-shortcuts.el)
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

### Issues

If you are having icons problems, run theses commands (inside emacs)

```
all-the-icons-install-fonts
```
```
nerd-icons-install-fonts
```
