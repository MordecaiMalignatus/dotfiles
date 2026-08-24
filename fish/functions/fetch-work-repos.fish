function fetch-work-repos
  for dir in ~/src/*
    if test -d $dir
      if string match -qe "_" $dir
        continue
      end
      echo (set_color green) "updating $dir" (set_color normal)
      fish -c "pushd $dir
      git fetch -a --prune &> /dev/null
      set -l branch (git-main)
      git pull origin \"$branch:$branch\" &> /dev/null
      popd" &
    end
  end
end
