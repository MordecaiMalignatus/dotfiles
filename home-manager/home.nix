{ config, pkgs, ... }:

let
  userdata = import ./userdata.nix;
  dotfiles = name: "${userdata.homeDirectory}/dotfiles/${name}";
in {
  # nodoxxpls.
  home.username = userdata.username;
  home.homeDirectory = userdata.homeDirectory;

  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.
  programs.home-manager.enable = true;

  home.packages = [
    pkgs.visidata
    pkgs.vim
    pkgs.pgcli
    pkgs.jless
    pkgs.gron
  ];

  home.file = {
    ".vimrc" = {
      source = dotfiles ".vimrc";
      onChange = "vim +PluginInstall +qall";
    };
    ".tmux.conf".source = dotfiles ".tmux.conf";
    ".hammerspoon".source = dotfiles ".hammerspoon";

    ".config/fish".source = dotfiles "fish";
    ".config/bat".source = dotfiles "bat";
  };

  # Launchd configuration, these are per-user daemons.
  # Documentation: https://nix-community.github.io/home-manager/options.html#opt-launchd.enable
  launchd.agents.fetch-work-repos = {
    enable = true;
    config = {
      ProgramArguments = ["/opt/homebrew/bin/fish" "-c" "fetch-work-repos"];
      StartInterval = (30 * 60); # Interval is in seconds, start every 30 minutes.
    };
  };

  home.sessionVariables = {
    # EDITOR = "emacs";
  };
}
