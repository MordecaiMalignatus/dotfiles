set -gx EDITOR (which vim)
set -gx BROWSER (which firefox)

# GO stuff.
set -gx GOPATH ~/go
set -gx GO11MODULE on

# Fix locales because locales always suck
set -gx LC_ALL "en_US.UTF-8"
set -gx LANG "en_US.UTF-8"
set -gx LANGUAGE "en_US.UTF-8"

# Aliases
alias k=kubectl
alias ist=istioctl

fish_add_path ~/.cargo/bin
fish_add_path ~/dotfiles/scripts
fish_add_path ~/go/bin/
fish_add_path ~/.local/bin
fish_add_path ~/.local/elixir-ls
fish_add_path ~/projects/shed/scripts
fish_add_path /usr/local/opt/curl/bin

if test (hostname) = "albatross"
        if isatty
                sq
        end
end

