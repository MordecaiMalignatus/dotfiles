# Defined in /var/folders/g7/r4g4b9_n1rqd3gk8fdq9d6wm0000gn/T//fish.hbWlq0/upgrade.fish @ line 2
function upgrade
	brew upgrade
  brew cask upgrade

  echo "Updating Vim..."
  vim +PluginUpdate +qa

  echo "Updating dotfiles..."
  pushd ~/dotfiles/
  git pull
  popd
end
