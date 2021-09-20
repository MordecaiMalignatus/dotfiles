set -gx EDITOR (which vim)
set -gx BROWSER (which firefox)
set -gx BAT_THEME "base16"

# Aliases
alias k=kubectl
alias ist=istioctl
alias tf=terraform

source /usr/local/opt/asdf/asdf.fish

fish_add_path ~/.cargo/bin
fish_add_path ~/dotfiles/scripts
fish_add_path ~/go/bin/
fish_add_path ~/.local/bin
fish_add_path ~/projects/wf/scripts

if test (hostname) = "albatross"
        sq
end

source "/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.fish.inc"
