function upgrade
    echo (set_color green)"Upgrading Packages..."(set_color normal)
    brew upgrade

    if test (hostname) = "ALT01896"
      echo (set_color green)"Upgrading Vault..."(set_color normal)
      vault wf upgrade
    end

    echo (set_color green)"Updating gcloud CLI..."(set_color normal)
    gcloud components update

    echo (set_color green)"Fetching work repos..."(set_color normal)
    fetch-work-repos

    echo (set_color green)"Updating dotfiles..."(set_color normal)
    pushd ~/dotfiles/
    git pull
    popd
end
