function fetch-work-repos
    set -l srcfiles (fd --maxdepth=1 --type directory . "$HOME/src/")
    set -l projectfiles (fd --maxdepth=1 --type directory . "$HOME/projects/")
    set -a srcfiles $projectfiles

    for dir in $srcfiles
        pushd $dir
        git fetch --all --prune --quiet
        popd
    end
end
