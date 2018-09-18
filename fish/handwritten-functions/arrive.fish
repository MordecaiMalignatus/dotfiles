# Defined in /tmp/fish.6twK6I/arrive.fish @ line 2
function arrive
	logrs "Arrived at work"
  start-ssh          # blocks until entered
  sudo -v
  sh ~/dotfiles/xrandr/work-screens.sh
  upgrade
  fetch-work-repos
  emacs &
  worktimecalc.py
  inbox-status
end
