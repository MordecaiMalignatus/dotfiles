# Work-specific config. Currently, this is for Stripe.
# As recommended in go/fish-shell
source (rbenv init -|psub)
source ~/stripe/space-commander/bin/sc-env-activate.fish
functions -e fish_right_prompt

## Work sources
fish_add_path "/Applications/Visual Studio Code.app/Contents/Resources/app/bin"
fish_add_path "$HOME/.rbenv/shims"
fish_add_path "$HOME/.rbenv/bin"
fish_add_path "$HOME/stripe/password-vault/bin"
fish_add_path "$HOME/stripe/space-commander/bin"
fish_add_path "$HOME/stripe/henson/bin"
