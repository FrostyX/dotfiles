{ lib, config, ... }:

lib.mkIf (builtins.getEnv "HOSTNAME" == "pop-os") {
  programs.git.enable = true;

  home.file = {
    ".bashrc" = {
      source = ".bashrc";
    };

    ".config/alacritty" = {
      source = ".config/alacritty";
      recursive = true;
    };

    ".config/cava" = {
      source = ".config/cava";
      recursive = true;
    };

    ".config/home-manager" = {
      source = ".config/home-manager";
      recursive = true;
    };

    ".config/khal" = {
      source = ".config/khal";
      recursive = true;
    };

    ".config/qtile" = {
      source = ".config/qtile";
      recursive = true;
    };

    ".config/rofi" = {
      source = ".config/rofi";
      recursive = true;
    };

    ".config/vlc" = {
      source = ".config/vlc";
      recursive = true;
    };

  };

}
