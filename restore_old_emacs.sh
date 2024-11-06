emacs_files=(".emacs.d" ".emacs.default" ".emacs-profiles.el" ".emacs.tiny")
new_old_dir="~/old_emacs/config1/"
echo "Restoring old emacs ..."
mkdir $new_old_dir

for file in "${emacs_files[@]}"; do
    mv ~/$file ~/old_emacs/config1/
    mv ~/old_emacs/$file ~/
done

echo "Done."
