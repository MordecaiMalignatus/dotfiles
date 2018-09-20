# Defined in /tmp/fish.5aFL5f/upgrade.fish @ line 2
function upgrade
	echo (set_color green)"Upgrading Packages"(set_color normal)
	switch (uname -s)
        case Linux
            sudo pacman -Syu --noconfirm
            
        case Darwin
            brew upgrade
    end

    echo (set_color green)"Updating dotfiles..."(set_color normal)
    pushd ~/dotfiles/
    git pull
    popd
end
