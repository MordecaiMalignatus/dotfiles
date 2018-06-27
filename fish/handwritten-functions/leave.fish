# Defined in /tmp/fish.gSMBy3/leave.fish @ line 2
function leave
  sudo -v
  loggrs "leaving work"
  i3lock -et -i "/home/az/wallpapers/lockscreen.png"
  sh ~/dotfiles/xrandr/only-laptop-screen.sh
  sudo systemctl hibernate
end
