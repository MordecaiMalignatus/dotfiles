DIR=dirname "$0"

# Bare dotfiles and folders
ln -s $DIR/.vimrc     $HOME/.vimrc
ln -s $DIR/.emacs.d   $HOME/.emacs.d
ln -s $DIR/.tigrc     $HOME/.tigrc
ln -s $DIR/.tmux.conf $HOME/.tmux.conf
ln -s $DIR/.zshrc     $HOME/.zshrc
ln -s $DIR/.gitconfig $HOME/.gitconfig

## .config folders.
mkdir -p ~/.config/
ln -s $DIR/i3         $HOME/.config/i3
ln -s $DIR/xrandr     $HOME/.config/xrandr
ln -s $DIR/fish       $HOME/.config/fish
ln -s $DIR/ranger     $HOME/.config/ranger
