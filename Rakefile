task default: [:emacs, :vim]

task :emacs do
  sh 'ln -s $HOME/dotfiles/.emacs.d $HOME/'
end

task :vim do
  sh 'ln -s $HOME/dotfiles/.vimrc $HOME/'
  sh 'vim +PluginUpdate +qa'
end
