set -g EDITOR (which vim)
set -g BROWSER (which firefox)

bind \cf forward-word

status --is-interactive; and source (pyenv init -|psub)

eval (direnv hook fish)

# Call sq during shell init. 
if isatty 
  sq status
end
