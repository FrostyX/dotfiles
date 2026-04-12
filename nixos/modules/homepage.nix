{ config, pkgs, ... }: {
  services.homepage-dashboard = {
    enable = true;
    openFirewall = true;
    listenPort = 8082;
    allowedHosts = "home.abathur.cz";
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
        # If this host is exposed via CloudFlare Tunnels, don't forget to
        # add a "Published application route" for every new service here.
        # CloudFlare -> Zero Trust -> Networks -> Connectors -> Cloudflare Tunnels
        Services = [
          { Radarr = { href = "https://radarr.abathur.cz"; }; }
          { Sonarr = { href = "https://sonarr.abathur.cz"; }; }
          { Lidarr = { href = "https://lidarr.abathur.cz"; }; }
          { Bazarr = { href = "https://bazarr.abathur.cz"; }; }
          { Prowlarr = { href = "https://prowlarr.abathur.cz"; }; }
          { Transmission = { href = "https://transmission.abathur.cz"; }; }
          { Jellyfin = { href = "https://jellyfin.abathur.cz"; }; }
          { Syncthing = { href = "#"; }; }
          { NextCloud = { href = "https://nextcloud.abathur.cz"; }; }
          { Cockpit = { href = "#"; }; }
          { Kuma = { href = "https://kuma.abathur.cz"; }; }
        ];
      }
    ];
  };
}
