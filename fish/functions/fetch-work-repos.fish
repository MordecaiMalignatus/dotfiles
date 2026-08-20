# Defined in /tmp/fish.Pf6slL/fetch-work-repos.fish @ line 2
function fetch-work-repos
  for dir in ~/src/*
    if test -d $dir
      if string match -qe "_" $dir
        continue
      end
      echo (set_color green) "updating $dir" (set_color normal)
      fish -c "pushd $dir
      git fetch -a --prune
      set -l branch (git-main)
      git pull origin \"$branch:$branch\"
      popd" &
    end
  end
end
