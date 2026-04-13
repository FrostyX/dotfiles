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
    ./hardware/fizzlebang.nix
    ./modules/arr.nix
    ./modules/transmission.nix
    ./modules/nextcloud.nix
    ./modules/jellyfin.nix
    ./modules/uptime-kuma.nix
    ./modules/homepage.nix
    ./modules/cloudflared.nix
  ];

  environment.systemPackages = with pkgs; [
    htop
    iotop
    ncdu
    curl
    vim
    lsof
  ];

  console.keyMap = "cz-qwertz";

  # Configuration for when building a VM and running it locally
  virtualisation.vmVariant.virtualisation = {
    memorySize = 2048;
    forwardPorts = [
      # Connect from the host system with:
      # ssh -p 2222 jkadlcik@127.0.0.1
      { from = "host"; host.port = 2222; guest.port = 22; }
    ];
  };


  services.openssh.enable = true;

  system.stateVersion = "23.11";
}
