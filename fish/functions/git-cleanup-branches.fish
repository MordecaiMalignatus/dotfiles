function git-cleanup-branches
        git branch --merged | grep -v '\*\|main\|master' | xargs -n1 git branch --delete
end
