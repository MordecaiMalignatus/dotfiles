# Defined in /tmp/fish.0Ra4l7/fetch-work-repos.fish @ line 2
function fetch-work-repos
	for dir in ~/work/*
    if test -d $dir 
      echo (set_color green) "updating $dir" (set_color normal)
      pushd $dir
      git fetch -a --prune > /dev/null &
      popd
    else
      echo "skipping $dir"
    end
  end
end
