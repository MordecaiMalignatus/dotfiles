# Defined in /tmp/fish.EbZkri/leave.fish @ line 2
function leave
	sudo -v
	logrs "leaving work"
  i3lock -et -i "/home/az/wallpapers/lockscreen.png"; and sh ~/dotfiles/xrandr/only-laptop-screen.sh; and sudo systemctl hibernate
end
