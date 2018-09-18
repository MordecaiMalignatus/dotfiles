# Defined in /tmp/fish.pmozHi/upgrade.fish @ line 2
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
end
