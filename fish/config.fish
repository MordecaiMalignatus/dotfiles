set -gx EDITOR (which vim)
set -gx BROWSER (which firefox)
set -gx BAT_THEME "GitHub"

# Aliases
alias k=kubectl
alias ist=istioctl
alias tf=terraform

source /usr/local/opt/asdf/asdf.fish

fish_add_path ~/.cargo/bin

if [ -f "$HOME/google-cloud-sdk/path.fish.inc" ]; . "$HOME/google-cloud-sdk/path.fish.inc"; end
