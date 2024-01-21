
echo "Adding the aliases to your dot config."
PS3="Please select your terminal interpretor : "
options=("BASH" "ZSH")
select commitPrefix in "${options[@]}"; do
    case $commitPrefix in
        "ZSH")
            echo "alias ne=\'emacsclient -nw -s tiny\'" >> ~/.zshrc
            echo "alias VSemacs=\'emacsclient -nw\'" >> ~/.zshrc
            source ~/.bashrc
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
