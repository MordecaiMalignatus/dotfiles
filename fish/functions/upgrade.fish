# Defined in /tmp/fish.qqqEIr/upgrade.fish @ line 2
function upgrade
	echo "Upgrading Packages"
	switch (uname -s)
        case Linux
            sudo pacman -Syu
            
        case Darwin
            brew upgrade
    end

    echo "Updating Vim..."
    vim +PluginUpdate +qa

    echo "Updating dotfiles..."
    pushd ~/dotfiles/
    git pull
    popd
end
