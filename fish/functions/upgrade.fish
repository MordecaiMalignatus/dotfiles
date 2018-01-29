# Defined in /var/folders/g7/r4g4b9_n1rqd3gk8fdq9d6wm0000gn/T//fish.G8Gx3i/upgrade.fish @ line 2
function upgrade
	echo "Upgrading Packages"
	switch (uname -s)
        case Linux
            sudo pacman -Syu
            
        case Darwin
            brew upgrade
    end

    echo "Updating dotfiles..."
    pushd ~/dotfiles/
    git pull
    popd
end
