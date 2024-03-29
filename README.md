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
1. **After each computer restart** ->run the provided "restart_emacs.sh" script, this will start or restart the daemons, i would suggest adding this to your autostart on your computer (the install script will run it for you the first time as he need to load all the packages but this script will need to be run each time you reeboot your computer)
2. VS CODE like editor, this config runs on the "emacs" daemon, in order to start an instance use the command : `VSemacs`
3. Tiny instance, this config runs on the "emacs tiny" daemon, starting an instance like the previous would be stupid, you could use it like this `ne "file_name"`

>[!IMPORTANT]
>The LSP mode (the thing that shows you warnings) runs off clang (that's why you install it) to modifie the flags it compiles with (add more warnings or a -I./include) copy the the (compile_flags.txt) in the root of your repository


## Keybings
If you are unfamiliar with emacs's base keybindings i would suggest reading this : [emacs cheatsheet](https://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf). (please use ctr-y, ctr-w and alt-w, for paste, cut, copy at least)
As for the config, it contains custom keybinds inside the [keybinds file](.emacs.default/custom/keyboard-shortcuts.el)
The most usefull are :
1. `ctr-t` : opens file browser
2. `ctr-c ctr-t` : change the file browser directory
3. `ctr-c ctr-s` : change the file browser workspace
4. `ctr-c ctr-e` : edit the file browser workspaces
5. `ctr-c ctr-z` : create a file browser workspace
6. `ctr-c ctr-d` : enter file manager mode
7. `ctr-c r` : replace all string by another
8. `ctr-c v` : allows copy in the vterm window
9. `ctr-n` : opens a new tab (if you want the vs code tab experience)
10. `ctr-q` : close said tab
11. `ctr-x t f` : looks for a file in the file browser
12. `ctr-x t b` : bookmarks a file
13. `ctr-x g` : git status of project

### Issues

## LSP MODE
If you are pompted with enable to find language for buffer, do `tab` `return` it will download the right language server

If you are having typescript related lsp mode errors, run this command
```
sudo npm i -g typescript-language-server typescript
```

## Icons
If you are having icons problems, run theses commands (inside emacs)

```
all-the-icons-install-fonts
```
```
nerd-icons-install-fonts
```
