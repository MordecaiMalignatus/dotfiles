# Defined in /tmp/fish.Pf6slL/fetch-work-repos.fish @ line 2
function fetch-work-repos
  for dir in ~/stripe/*
    if test -d $dir 
      echo (set_color green) "updating $dir" (set_color normal)
      pushd $dir
      git fetch -a --prune
      git checkout master
      git pull
      git switch -
      popd
    else
      echo "skipping $dir"
    end
  end
end
