# Defined in /tmp/fish.yYEidO/leave.fish @ line 2
function leave
	sudo -v
  logrs "leaving work"
  i3lock -et -i "/home/az/wallpapers/lockscreen.png"
  sh ~/dotfiles/xrandr/only-laptop-screen.sh
  sudo shutdown now
end
