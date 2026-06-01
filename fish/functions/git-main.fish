function git-main
        git branch --list master main | tr -d '* ' | head -n1
end
