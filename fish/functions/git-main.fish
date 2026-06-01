function git-main
        git branch --list master main | tr -d '* '
end
