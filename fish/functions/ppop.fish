# Defined in /var/folders/g7/r4g4b9_n1rqd3gk8fdq9d6wm0000gn/T//fish.LZPAvy/ppop.fish @ line 1
function ppop
	pockyt get -n 1 -r oldest -s unread -o browser | pockyt mod -a 1 -i redirect
end
