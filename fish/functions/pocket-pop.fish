# Defined in /var/folders/g7/r4g4b9_n1rqd3gk8fdq9d6wm0000gn/T//fish.tQgrOq/pocket-pop.fish @ line 2
function pocket-pop
	pockyt get -n 1 -s unread -o browser | pockyt mod -a 1 -i redirect
end
