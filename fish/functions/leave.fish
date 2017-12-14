# Defined in /tmp/fish.FPiQGY/leave.fish @ line 2
function leave
	logrs "leaving work"
    i3lock -et -i "/home/az/wallpapers/lockscreen.png"; and systemctl hibernate
end
