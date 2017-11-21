# Defined in /var/folders/g7/r4g4b9_n1rqd3gk8fdq9d6wm0000gn/T//fish.y3qFxE/upgrade.fish @ line 2
function upgrade
	brew upgrade
  echo "updating vim..."
  vim +PluginUpdate +qa
end
