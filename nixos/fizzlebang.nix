{ config, pkgs, ... }: {
  networking.hostName = "fizzlebang";

  users.users.jkadlcik = {
    isNormalUser = true;
    password = "test";
  };

  imports = [
    ./modules/arr.nix
    ./modules/transmission.nix
    ./modules/nextcloud.nix
    ./modules/jellyfin.nix
    ./modules/uptime-kuma.nix
    ./modules/homepage.nix
  ];

  services.openssh.enable = true;

  system.stateVersion = "23.11";
}
