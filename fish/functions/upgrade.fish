# Defined in - @ line 2
function upgrade
	brew upgrade

    echo "Updating Vim..."
    vim +PluginUpdate +qa

    echo "Updating dotfiles..."
    pushd ~/dotfiles/
    git pull
    popd
end
