function fetch-work-repos
    set -l srcfiles (~/src/*)
    set -l projectfiles (~/projects/*)
    set -a srcfiles projectfiles

    for dir in $srcfiles
        pushd $dir
        git fetch --all --prune --quiet
        popd
    end
end
