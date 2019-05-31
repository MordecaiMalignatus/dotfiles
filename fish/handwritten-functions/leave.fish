# Defined in /tmp/fish.zIUHW0/leave.fish @ line 2
function leave
	sudo -v
  logrs "leaving work"
  sudo shutdown now
end
