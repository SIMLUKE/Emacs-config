# Emacs Config
>[!CAUTION]
>All previous emacs related info has been moved to the wiki page of the repository, feel free to check out.

## Install
Just execute my script (will take some time)

(if asked project root, awnser ".")

(if asked to compile vterm say yes)


## Update
Just execute my script (will take some time)

### Usage
1. **After each computer restart** ->run the provided "restart_emacs.sh" script, this will start or restart the daemons, i would suggest adding this to your autostart on your computer (the install script will run it for you the first time as he need to load all the packages but this script will need to be run each time you reeboot your computer)
2. VS CODE like editor, this config runs on the emacs daemon, in order to open a frame, use the command : `VSemacs`
3. Tiny instance, this config runs on the "emacs tiny" daemon, it's meant for console use and to open single files in the terminal, like : `ne "file_name"`

>[!IMPORTANT]
>The LSP mode (the thing that shows you warnings) runs off clang (that's why you install it) to modifie the flags it compiles with (add more warnings or a -I./include) copy the the (compile_flags.txt) in the root of your repository

