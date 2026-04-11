{ config, pkgs, ... }: {
  networking.hostName = "fizzlebang";

  users.users.jkadlcik = {
    isNormalUser = true;
    password = "test";
  };

  services.radarr = {
    enable = true;
    openFirewall = true;
  };

  services.bazarr = {
    enable = true;
    openFirewall = true;
  };

  services.prowlarr = {
    enable = true;
    openFirewall = true;
  };

  services.transmission = {
    enable = true;
    package = pkgs.transmission_4;
    openFirewall = true;
    settings = {
      download-dir = "/mnt/data1/downloads/done";
      incomplete-dir = "/mnt/data1/downloads/incomplete";
      rpc-authentication-required = true;
      rpc-username = "transmission";
      rpc-whitelist-enabled = false;
      rename-partial-files = false;
      lpd-enabled = true;
    };
    # rpc-password should go in credentialsFile to keep it out of the nix store:
    # credentialsFile = "/run/secrets/transmission-credentials";
  };

  services.nextcloud = {
    enable = true;
    package = pkgs.nextcloud32;
    hostName = "localhost";
    home = "/mnt/data2/nextcloud";
    https = false;
    database.createLocally = true;
    config = {
      dbtype = "pgsql";
      adminpassFile = "/run/secrets/nextcloud-adminpass";
    };
  };

  # Nextcloud requires nginx; move it off port 80 so homepage can use it
  services.nginx = {
    enable = true;
    virtualHosts."localhost" = {
      listen = [{ addr = "0.0.0.0"; port = 8080; ssl = false; }];
    };
  };

  services.jellyfin = {
    enable = true;
    openFirewall = true;
  };

  services.uptime-kuma = {
    enable = true;
    settings.HOST = "0.0.0.0";
    settings.PORT = "3001";
  };

  services.homepage-dashboard = {
    enable = true;
    openFirewall = true;
    listenPort = 8082;
    settings = {
      title = "Abathur";
      theme = "light";
      color = "white";
      hideVersion = true;
      target = "_self";
    };
    widgets = [
      {
        greeting = {
          text_size = "4xl";
          text = "Abathur";
        };
      }
    ];
    services = [
      {
        Services = [
          { Radarr = { href = "http://localhost:7878"; }; }
          { Bazarr = { href = "http://localhost:6767"; }; }
          { Prowlarr = { href = "http://localhost:9696"; }; }
          { Transmission = { href = "http://localhost:9091"; }; }
          { Jellyfin = { href = "http://localhost:8096"; }; }
          { Syncthing = { href = "#"; }; }
          { NextCloud = { href = "http://localhost:8080"; }; }
          { Cockpit = { href = "#"; }; }
          { Kuma = { href = "http://localhost:3001"; }; }
        ];
      }
    ];
  };

  system.stateVersion = "23.11";
}
