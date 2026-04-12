{ config, pkgs, ... }: {
  networking.hostName = "fizzlebang";

  users.users.jkadlcik = {
    isNormalUser = true;
    password = "test";
    extraGroups = [ "wheel" ];
  };

  imports = [
    ./modules/arr.nix
    ./modules/transmission.nix
    ./modules/nextcloud.nix
    ./modules/jellyfin.nix
    ./modules/uptime-kuma.nix
    ./modules/homepage.nix
    ./modules/cloudflared.nix
  ];

  services.openssh.enable = true;

  system.stateVersion = "23.11";
}
