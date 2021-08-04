function fetch-work-repos
  for dir in ~/projects/*
    pushd $dir
    git fetch --all --prune --quiet
    popd
  end
end
