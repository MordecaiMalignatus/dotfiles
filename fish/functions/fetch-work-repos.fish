# Defined in /tmp/fish.Pf6slL/fetch-work-repos.fish @ line 2
function fetch-work-repos
  for dir in ~/src/*
    if test -d $dir
      echo (set_color green) "updating $dir" (set_color normal)
      pushd $dir
      git fetch -a --prune
      set -l branch (git-main)
      if test (git branch --show-current) != $branch
        git stash
        git switch $branch
        git pull --rebase --autostash
        git switch -
        git stash pop
      end
      popd
    else
      echo "skipping $dir"
    end
  end
end
