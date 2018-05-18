# Defined in /tmp/fish.6Hse7R/arrive.fish @ line 2
function arrive
	logrs "Arrived at work"
  ssh-add          # blocks until entered
  sh ~/dotfiles/xrandr/work-screens.sh
  upgrade
  fetch-work-repos
  task next
end
