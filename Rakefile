task :default => :list

task :list do
  puts "Supply dotfiles to be installed. See more with `rake -AT`.`"
end

task :all => [:tmux, :emacs, :vim, :fish]

task :tmux do
  link_home ".tmux.conf"
end

task :emacs do
  link_home ".emacs.d"
end

task :vim do
  link_home ".vimrc"
end

task :fish do
  link_config "fish"
end

task :alacritty do
  link_config "alacritty"
end

def link_home(file)
  sh "ln -s $PWD/#{file} $HOME/#{file}"
end

def link_config(file)
  sh "ln -s $PWD/#{file} $HOME/.config/#{file}"
end
