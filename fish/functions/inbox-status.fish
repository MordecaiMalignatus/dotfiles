# Defined in /var/folders/g7/r4g4b9_n1rqd3gk8fdq9d6wm0000gn/T//fish.Ek2NGU/inbox-status.fish @ line 2
function inbox-status
	set -lx nn_inbox_count (ls $NN_INBOX | wc -l)
	echo (set_color -o green) "You have $nn_inbox_count notes waiting for you." (set_color -o normal)
end
