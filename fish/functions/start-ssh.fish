# Defined in /tmp/fish.4Asxpw/start-ssh.fish @ line 1
function start-ssh
	eval (ssh-agent -c)
  ssh-add
end
