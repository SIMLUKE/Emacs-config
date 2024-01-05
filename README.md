# Emacs Config
>[!CAUTION]
>Please read this page carrefully, you need to change your way of using emacs in oder to maximize the potential of this config.

## Emacs Daemons
### Quick explation
This config was made in order to run on daemons, this allows emacs to be instantly opened once the daemon is started, doing otherwise would be stupid.

### Usage
1. **BEFORE EVERYTHING** run the provided "restart_emacs.sh" script, this will start or restart the daemons, i would suggest adding this to your autostart on your computer (the install script will run it for you the first time as he need to load all the packages)
2. VS CODE like editor, this config runs on the "emacs" daemon, in order to start an instance use the command : `emacsclient -c`
3. Tiny instance, this config runs on the "emacs tiny" daemon, starting an instance like the previous would be stupid, this was made to open one file at a timei would HIGHLY suggest you add this code to your ".zshrc" or ".bashrc" config :
```
ne() {
    emacsclient -c -s tiny -a "" "$1" &
    disown
}
```
