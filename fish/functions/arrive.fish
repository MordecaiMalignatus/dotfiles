# Defined in /tmp/fish.KcZ7Rd/arrive.fish @ line 2
function arrive
	logrs "Arrived at work"
    ssh-add          # blocks until entered
    sudo pacman -Syu # also blocks until sudo is entered
    fetch-work-repos
    task next
end
