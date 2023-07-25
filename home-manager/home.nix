{ config, pkgs, ... }:

let
  userdata = import ./userdata.nix;
  dotfiles = "${userdata.homeDirectory}/dotfiles";
in
{
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
  ];

  home.file = {
    ".vimrc".source     = "${dotfiles}/.vimrc";
    ".tmux.conf".source = "${dotfiles}/.tmux.conf";
    ".vimrc" = {
      source = dotfiles ".vimrc";
      onChange = "vim +PluginInstall +qall";
    };
  };

  home.sessionVariables = {
    # EDITOR = "emacs";
  };


}
