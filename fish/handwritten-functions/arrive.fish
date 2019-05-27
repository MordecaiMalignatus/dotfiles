# Defined in /tmp/fish.nB77mJ/arrive.fish @ line 2
function arrive
  logrs "Arrived at work"
  emacs &; disown
  sh ~/dotfiles/xrandr/work-screens.sh
  i3-msg workspace Nerdery
  fetch-work-repos
  worktimecalc.py
end
