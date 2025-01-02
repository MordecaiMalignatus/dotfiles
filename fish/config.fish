set -gx EDITOR (which vim)

# GO stuff.
set -gx GOPATH ~/go
set -gx GO11MODULE on

if type -q home-manager
    babelfish < ~/.nix-profile/etc/profile.d/hm-session-vars.sh | source
    any-nix-shell fish --info-right | source
end

status --is-interactive; and rbenv init - fish | source
if type -q gcloud
    source /opt/homebrew/share/google-cloud-sdk/path.fish.inc
end

if work-machine-p
    source ~/.config/fish/work.fish
    alias git=/usr/local/bin/stripe-git
end

# if test -e ~/.ssh/id_ed25519
#     ssh-add ~/.ssh/id_ed25519
# end

# Fix locales because locales always suck
set -gx LC_ALL "en_US.UTF-8"
set -gx LANG "en_US.UTF-8"
set -gx LANGUAGE "en_US.UTF-8"

# Aliases
alias k=kubectl
alias exa=eza # Legacy compat for old scripts.

fish_add_path ~/.cargo/bin
fish_add_path ~/dotfiles/scripts
fish_add_path ~/go/bin/
fish_add_path ~/.local/bin
fish_add_path ~/.local/elixir-ls
fish_add_path /opt/homebrew/bin


# BEGIN opam configuration
# This is useful if you're using opam as it adds:
#   - the correct directories to the PATH
#   - auto-completion for the opam binary
# This section can be safely removed at any time if needed.
test -r '/Users/az/.opam/opam-init/init.fish' && source '/Users/az/.opam/opam-init/init.fish' > /dev/null 2> /dev/null; or true
# END opam configuration

# bun
set --export BUN_INSTALL "$HOME/.bun"
set --export PATH $BUN_INSTALL/bin $PATH
