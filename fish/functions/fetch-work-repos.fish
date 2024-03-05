function fetch-work-repos
    set -l srcfiles (fd --maxdepth=1 --type directory . "$HOME/src/" 2>/dev/null)
    set -l projectfiles (fd --maxdepth=1 --type directory . "$HOME/projects/" 2>/dev/null)
    set -a srcfiles $projectfiles

    if work-machine-p
        set -l workfiles (fd --maxdepth=1 --type directory . "$HOME/stripe/")
        set -a srcfiles $workfiles
    end

    for dir in $srcfiles
        print_green "Fetching $dir"
        pushd $dir
        git fetch --all --prune --quiet
        popd
    end
end
