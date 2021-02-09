set -g EDITOR (which vim)
set -g BROWSER (which firefox)

# Aliases
alias k=kubectl
alias ist=istioctl

# Call sq during shell init.
if isatty
  sq status
end

source /usr/local/opt/asdf/asdf.fish
