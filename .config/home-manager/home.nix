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

  # Fix clicking on links in Flatpak applications (e.g. Slack, Element) by
  # exposing Nix profile paths to systemd user services
  # (e.g. xdg-desktop-portal) so they can find and launch Nix-installed
  # applications like zen-beta.
  systemd.user.sessionVariables = {
    PATH = builtins.concatStringsSep ":" [
      "$HOME/.nix-profile/bin"
      "/nix/var/nix/profiles/default/bin"
      "\${PATH}"
    ];
    XDG_DATA_DIRS = builtins.concatStringsSep ":" [
      "$HOME/.nix-profile/share"
      "/nix/var/nix/profiles/default/share"
      "\${XDG_DATA_DIRS}"
    ];
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

  # I think it works it just requires session restart
  home.pointerCursor = {
    name = "breeze_cursors";
    package = pkgs.kdePackages.breeze;
    size = 24;
    gtk.enable = true;
  };

  imports = [
    ./claude.nix
    ./codex.nix
    ./emacs.nix
    ./git.nix
    ./flatpak.nix
    ./xdg.nix
    ./zen.nix
    ./symlinks.nix
    ./future.nix
  ];

}
