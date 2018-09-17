# Defined in /tmp/fish.m5HTeT/ppop.fish @ line 2
function ppop
	pockyt get -n 1 -r oldest -s unread -o browser | pockyt mod -a 1 -i redirect
	nn
end
