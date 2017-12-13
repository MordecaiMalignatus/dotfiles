# cd into directory of this very script
# (otherwise the creating of links won't work!)
# http://stackoverflow.com/a/246128/4568748
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$DIR" || exit 1

# Bare dotfiles and folders
ln -s $DIR/.vimrc     $HOME/.vimrc
ln -s $DIR/.emacs.d   $HOME/.emacs.d
ln -s $DIR/.tigrc     $HOME/.tigrc
ln -s $DIR/.tmux.conf $HOME/.tmux.conf
ln -s $DIR/.zshrc     $HOME/.zshrc
ln -s $DIR/.gitconfig $HOME/.gitconfig

## .config folders.
mkdir -p ~/.config/
ln -s $DIR/i3         $HOME/.config/
ln -s $DIR/xrandr     $HOME/.config/
ln -s $DIR/fish       $HOME/.config/
ln -s $DIR/ranger     $HOME/.config/
