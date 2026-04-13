{ config, pkgs, ... }: {
  age.secrets.cloudflared.file = ../secrets/cloudflared.age;

  services.cloudflared = {
    enable = true;
    tunnels."667affed-baec-4eed-9833-bf69869bce9a" = {
      credentialsFile = config.age.secrets.cloudflared.path;
      default = "http_status:404";
    };
  };
}
