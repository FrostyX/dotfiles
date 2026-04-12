{ config, pkgs, ... }: {
  networking.hostName = "fizzlebang";

  users.users.jkadlcik = {
    isNormalUser = true;
    password = "test";
    extraGroups = [ "wheel" ];
  };

  # Open ports:
  #   22    SSH
  #   3001  Uptime Kuma
  #   6767  Bazarr
  #   7878  Radarr
  #   8082  Homepage
  #   8096  Jellyfin
  #   8384  Syncthing
  #   8686  Lidarr
  #   8080  Nextcloud (nginx)
  #   8989  Sonarr
  #   9090  Cockpit
  #   9091  Transmission
  #   9696  Prowlarr

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
