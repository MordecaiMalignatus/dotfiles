set -g EDITOR (which vim)
set -g BROWSER (which firefox)

# Aliases
alias k=kubectl
alias ist=istioctl
alias tf=terraform

source /usr/local/opt/asdf/asdf.fish

fish_add_path ~/.cargo/bin

if [ -f "$HOME/google-cloud-sdk/path.fish.inc" ]; . "$HOME/google-cloud-sdk/path.fish.inc"; end
