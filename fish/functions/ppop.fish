# Defined in /tmp/fish.aqBvgC/ppop.fish @ line 2
function ppop
	pockyt get -n 1 -r oldest -s unread -o browser | pockyt mod -a 1 -i redirect
end
