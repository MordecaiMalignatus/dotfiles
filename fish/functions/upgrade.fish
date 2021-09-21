function upgrade
    print_green "Upgrading Packages..."
    brew upgrade

    if test (hostname) = "ALT01896" 
      and test (math (date '+%j') % 7) = 0
      print_green "Upgrading Vault..."
      vault wf upgrade
    end

    print_green "Updating gcloud CLI..."
    gcloud components update --quiet

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
