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

  programs.git = {
    enable = true;

    # See ~/.config/git/config
    settings = {
      user = {
        name = "Jakub Kadlcik";
        email = "frostyx@email.cz";
      };
      alias = {
        lol = "log --graph --decorate --pretty=oneline --abbrev-commit";
        forcepull = "!git reset --hard main && git pull --rebase";
        foo = "log";
      };
    };
  };

  gtk = {
    enable = true;
    # See ~/.config/gtk-3.0/settings.ini
    # theme.name = "";
    # cursorTheme.name = "";
    iconTheme.name = "breeze";
  };


  # You might need to manually run
  # rm /home/jkadlcik/.local/share/applications/mimeapps.list
  xdg = {
    mimeApps.enable = true;
    configFile."mimeapps.list".force = true;

    # See /home/jkadlcik/.local/share/applications/mimeapps.list
    mimeApps.defaultApplications = {
      "text/plain" = "org.kde.kwrite.desktop";
      "text/html" = "zen-browser.desktop";
      "image/jpeg" = "org.gnome.eog.desktop";
      "image/png" = "org.gnome.eog.desktop";
      "image/webp" = "org.gnome.eog.desktop";
      "image/svg+xml" = "org.inkscape.Inkscape.desktop";
      "video/mp4" = "vlc.desktop";
      "video/quicktime" = "vlc.desktop";
      "video/x-msvideo" = "vlc.desktop";
      "video/x-matroska" = "vlc.desktop";
      "application/pdf" = "okularApplication_pdf.desktop";
      "x-scheme-handler/http" = "zen-browser.desktop";
      "x-scheme-handler/https" = "zen-browser.desktop";
    };
  };

  imports = [
    ./zen.nix
    ./future.nix
  ];

}
