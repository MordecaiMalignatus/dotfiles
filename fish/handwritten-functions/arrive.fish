# Defined in /tmp/fish.utvWl1/arrive.fish @ line 2
function arrive
	logrs "Arrived at work"
  ssh-add          # blocks until entered
  upgrade
  fetch-work-repos
  task next
end
