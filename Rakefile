task default: [:emacs, :vim, :git, :ghostty]

task :emacs do
  sh 'ln -s $HOME/dotfiles/.emacs.d $HOME/'
  sh 'touch ~/.emacs.d/custom.el'
end

task :vim do
  sh 'ln -s $HOME/dotfiles/.vimrc $HOME/'
  sh 'vim +PluginUpdate +qa'
end

task :git do
  sh 'ln -s $HOME/dotfiles/git-global-config $HOME/.gitconfig'
end

task :ghostty do
  sh 'mkdir -p "$HOME/Library/Application Support/com.mitchellh.ghostty"'
  sh 'ln -sf $HOME/dotfiles/ghostty/config.ghostty "$HOME/Library/Application Support/com.mitchellh.ghostty/config"'
end
