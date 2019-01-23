set -g EDITOR (which vim)
set -g BROWSER (which firefox)

bind \cf forward-word

if not set -q abbr_init
   set -U abbr_init t
   echo -n "Setting abbrevs"

   abbr tsh 'tmux.sh'

   echo -n "Set fish abbreviations..."
end