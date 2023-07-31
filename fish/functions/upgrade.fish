function upgrade
    print_green "Upgrading Packages..."
    brew upgrade

    print_green "Fetching work repos..."
    fetch-work-repos

    print_green "Updating dotfiles..."
    pushd ~/dotfiles/
    git stash
    git pull
    git stash pop
    popd

    print_green "Updating work music..."
    pushd ~/work-music/
    git pull
    rake dl
    popd

    sync-grimoire
end
