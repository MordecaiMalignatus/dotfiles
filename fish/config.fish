set -gx EDITOR (which vim)

# GO stuff.
set -gx GOPATH ~/go
set -gx GO11MODULE on

if type -q home-manager
    babelfish < ~/.nix-profile/etc/profile.d/hm-session-vars.sh | source
    any-nix-shell fish --info-right | source
end

if type -q gcloud
    source /opt/homebrew/share/google-cloud-sdk/path.fish.inc
end

if work-machine-p
    source ~/.config/fish/work.fish
end

# Fix locales because locales always suck
set -gx LC_ALL "en_US.UTF-8"
set -gx LANG "en_US.UTF-8"
set -gx LANGUAGE "en_US.UTF-8"

# Aliases
alias k=kubectl

fish_add_path ~/.cargo/bin
fish_add_path ~/dotfiles/scripts
fish_add_path ~/go/bin/
fish_add_path ~/.local/bin
fish_add_path ~/.local/elixir-ls
fish_add_path /opt/homebrew/bin
