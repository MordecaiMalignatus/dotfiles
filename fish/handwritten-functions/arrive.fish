# Defined in /tmp/fish.efFVbT/arrive.fish @ line 2
function arrive
	logrs "Arrived at work"
  sh ~/dotfiles/xrandr/work-screens.sh
  sh ~/dotfiles/scripts/i3-arrangement.sh
  upgrade
  fetch-work-repos
  emacs &
  worktimecalc.py
  inbox-status
end
