function upgrade
    echo (set_color green)"Upgrading Packages"(set_color normal)
    brew upgrade
    if test hostname = "ALT01896\n"
      echo (set_color green)"Upgrading Vault"(set_color normal)
      vault wf upgrade
    end

    echo (set_color green)"Updating dotfiles..."(set_color normal)
    pushd ~/dotfiles/
    git pull
    popd
end
