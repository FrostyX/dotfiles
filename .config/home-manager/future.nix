{ lib, config, hostname, ... }:

let
  topdir = "${config.home.homeDirectory}/.dotfiles";
  mkLink = path: config.lib.file.mkOutOfStoreSymlink "${topdir}/${path}";

  paths = [
    ".bashrc"
    ".config/alacritty"
    ".config/cava"
    ".config/home-manager"
    ".config/khal"
    ".config/qtile"
    ".config/rofi"
    ".config/vlc"
  ];
in
lib.mkIf (hostname == "pop-os") {
  home.file = builtins.listToAttrs (map (path: {
    name = path;
    value = { source = mkLink path; };
  }) paths);
}
