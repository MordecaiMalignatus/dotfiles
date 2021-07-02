set -g EDITOR (which vim)
set -g BROWSER (which firefox)

# Aliases
alias k=kubectl
alias ist=istioctl

source /usr/local/opt/asdf/asdf.fish

fish_add_path ~/.cargo/bin
