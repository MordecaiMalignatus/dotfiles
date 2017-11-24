# Defined in /var/folders/g7/r4g4b9_n1rqd3gk8fdq9d6wm0000gn/T//fish.xS4V23/update-dotfiles.fish @ line 1
function update-dotfiles
	pushd ~/dotfiles
  git pull
  popd
end
