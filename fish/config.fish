set -g EDITOR (which vim)
set -g BROWSER (which firefox)

bind \cf forward-word


status --is-interactive; and source (pyenv init -|psub)

if not set -q abbr_init
   set -U abbr_init t
   echo -n "Setting abbrevs"

   abbr tsh 'tmux.sh'

   echo -n "Set fish abbreviations..."
end
