#!/bin/bash

echo "Generating personnal config file"

conf_file=.emacs.default/own_conf/conf.el

cat ./own_conf_template/top.txt >>$conf_file

cat ./own_conf_template/middle.txt >>$conf_file

terminals=("alacritty" "gnome-terminal" "konsole" "xterm" "st" "kitty" "tilix" "xfce4-terminal" "urxvt" "hyper" "wezterm" "foot")

installed_terminals=()

echo "Checking installed terminals..."
for terminal in "${terminals[@]}"; do
    if which "$terminal" >/dev/null 2>&1; then
        installed_terminals+=("$terminal")
    fi
done

if [ ${#installed_terminals[@]} -eq 0 ]; then
    echo "No terminals found from the list. Please install one."
    exit 1
fi

echo "The following terminals are available on your system:"
for i in "${!installed_terminals[@]}"; do
    echo "$((i + 1))) ${installed_terminals[$i]}"
done

echo -n "Choose a terminal to launch (1-${#installed_terminals[@]}): "
read -r choice

if [[ "$choice" =~ ^[1-9][0-9]*$ ]] && ((choice > 0 && choice <= ${#installed_terminals[@]})); then
    chosen_terminal="${installed_terminals[$((choice - 1))]}"
    sed -i "s/\[TERM\]/$chosen_terminal/g" $conf_file
else
    echo "Invalid choice."
fi

read -p "Do you want to remove scroll bar ? (y/n): " answer
if [[ "$answer" =~ ^[Yy]$ ]]; then
    echo '(scroll-bar-mode -1)' >>$conf_file
fi

echo "Choose an option:"
echo "1) Right"
echo "2) Left"
read -p "Enter 1 or 2: " input

if [[ "$input" == "1" ]]; then
    echo "(setq treemacs-position 'right)" >>$conf_file
elif [[ "$input" == "2" ]]; then
    echo "(setq treemacs-position 'left)" >>$conf_file
else
    echo "Invalid choice. Please enter 1 or 2."
fi

cat ./own_conf_template/bot.txt >>$conf_file
echo "Done generating $conf_file."
