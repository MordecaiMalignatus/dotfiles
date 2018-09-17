# Defined in /tmp/fish.DkYQXL/arrive.fish @ line 2
function arrive
	logrs "Arrived at work"
  start-ssh          # blocks until entered
  sudo -v
  sh ~/dotfiles/xrandr/work-screens.sh
  upgrade
  fetch-work-repos
end
