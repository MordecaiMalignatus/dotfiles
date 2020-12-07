set -g EDITOR (which vim)
set -g BROWSER (which firefox)

status --is-interactive; and source (pyenv init -|psub)
status --is-interactive; and source (pyenv virtualenv-init -|psub)

# Aliases
alias k=kubectl
alias ist=istioctl

# Call sq during shell init.
if isatty
  sq status
end
