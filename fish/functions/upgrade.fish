# Defined in /var/folders/g7/r4g4b9_n1rqd3gk8fdq9d6wm0000gn/T//fish.4J19dm/upgrade.fish @ line 2
function upgrade
	echo (set_color green)"Upgrading Packages"(set_color normal)
	switch (uname -s)
        case Linux
            yes | sudo pacman -Syu
            
        case Darwin
            brew upgrade
    end

    echo (set_color green)"Updating dotfiles..."(set_color normal)
    pushd ~/dotfiles/
    git pull
    popd
    
    inbox-status
end
