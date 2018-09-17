# Defined in /tmp/fish.mmEUqX/arrive.fish @ line 2
function arrive
	logrs "Arrived at work"
  start-ssh          # blocks until entered
  sudo -v
  sh ~/dotfiles/xrandr/work-screens.sh
  upgrade
  fetch-work-repos
  echo (set_color -o green) (~/dotfiles/scripts/worktimecalc.py) (set_color -o normal)
end
