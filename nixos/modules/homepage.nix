{ config, pkgs, ... }: {
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
        # If this host is exposed via CloudFlare Tunnels, don't forget to
        # add a "Published application route" for every new service here.
        # CloudFlare -> Zero Trust -> Networks -> Connectors -> Cloudflare Tunnels
        Services = [
          { Radarr = { href = "http://localhost:7878"; }; }
          { Sonarr = { href = "http://localhost:8989"; }; }
          { Lidarr = { href = "http://localhost:8686"; }; }
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
}
