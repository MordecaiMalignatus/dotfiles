# Defined in /tmp/fish.soNF5j/arrive.fish @ line 2
function arrive
	logrs "Arrived at work"
  ssh-add          # blocks until entered
  sudo -v
  sh ~/dotfiles/xrandr/work-screens.sh
  upgrade
  fetch-work-repos
end
