ln -s ~/source/dotfiles/files/.emacs ~/.emacs

ln -s ~/source/dotfiles/files/i3/config ~/.config/i3/config
ln -s ~/source/dotfiles/files/i3/i3status.conf ~/.config/i3/i3status.conf
ln -s ~/source/dotfiles/files/i3/workspace-misc.json ~/.config/i3/workspace-misc.json
ln -s ~/source/dotfiles/files/i3/o.sh ~/.config/i3/o.sh
ln -s ~/source/dotfiles/files/i3/eject.sh ~/.config/i3/eject.sh

ln -s ~/source/dotfiles/files/.bash_profile ~/.bash_profile

ln -s ~/source/dotfiles/files/.gitconfig ~/.gitconfig

mkdir -p ~/.local/bin
cp ~/source/dotfiles/files/scripts/* ~/.local/bin
