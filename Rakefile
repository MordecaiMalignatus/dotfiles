task default: [:emacs, :vim]

task :emacs do
  sh 'ln -s $HOME/dotfiles/.emacs.d $HOME/'
  sh 'touch ~/.emacs.d/custom.el'
end

task :vim do
  sh 'ln -s $HOME/dotfiles/.vimrc $HOME/'
  sh 'vim +PluginUpdate +qa'
end
