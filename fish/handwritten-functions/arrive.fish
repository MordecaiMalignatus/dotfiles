# Defined in /tmp/fish.10V90o/arrive.fish @ line 2
function arrive
	logrs "Arrived at work"
  sudo -v
  sh ~/dotfiles/xrandr/work-screens.sh
  upgrade
  fetch-work-repos
  emacs &
  worktimecalc.py
  inbox-status
end
