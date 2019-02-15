# Defined in /tmp/fish.nB77mJ/arrive.fish @ line 2
function arrive
  logrs "Arrived at work"
  emacs &
  sh ~/dotfiles/xrandr/work-screens.sh
  sh ~/dotfiles/scripts/i3-arrangement.sh
  sh ~/dotfiles/scripts/open-auth-sites.sh
  i3-msg workspace Nerdery
  fetch-work-repos
  worktimecalc.py
end
