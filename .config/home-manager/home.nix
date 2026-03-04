{ lib, config, pkgs, ... }:

{
  home.username = "jkadlcik";
  home.homeDirectory = "/home/jkadlcik";
  home.stateVersion = "23.11";

  home.packages = with pkgs; [
    git
    ripgrep
  ];

  home.sessionVariables = {
    EDITOR = "vim";
  };

  home.file = {
  };

  programs.home-manager.enable = true;

  gtk = {
    enable = true;
    # See ~/.config/gtk-3.0/settings.ini
    # theme.name = "";
    # cursorTheme.name = "";
    iconTheme.name = "breeze";
  };


  imports = [
    ./git.nix
    ./xdg.nix
    ./zen.nix
    ./symlinks.nix
    ./future.nix
  ];

}
