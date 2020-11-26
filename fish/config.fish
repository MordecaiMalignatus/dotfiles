set -g EDITOR (which vim)
set -g BROWSER (which firefox)

bind \cf forward-word

status --is-interactive; and source (pyenv init -|psub)
status --is-interactive; and source (pyenv virtualenv-init -|psub)

eval (direnv hook fish)

# Aliases
alias k=kubectl
alias ist=istioctl

# Call sq during shell init.
if isatty
  sq status
end

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/az/Downloads/google-cloud-sdk/path.fish.inc' ]; . '/Users/az/Downloads/google-cloud-sdk/path.fish.inc'; end
