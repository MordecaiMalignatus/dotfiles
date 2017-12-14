# Defined in /tmp/fish.z5iHQj/arrive.fish @ line 1
function arrive
	logrs "Arrived at work"
    ssh-add          # blocks until entered
    sudo pacman -Syu # also blocks until sudo is entered
    fetch-work-repos
end
