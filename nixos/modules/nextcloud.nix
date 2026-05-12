{ config, pkgs, ... }: {
  age.secrets.nextcloud.file = ../secrets/nextcloud.age;

  services.nextcloud = {
    enable = true;
    package = pkgs.nextcloud32;
    hostName = "localhost";
    home = "/mnt/data1/nextcloud";
    https = false;
    database.createLocally = true;
    config = {
      dbtype = "pgsql";
      adminpassFile = config.age.secrets.nextcloud.path;
    };
    settings = {
      trusted_domains = [ "localhost" "nextcloud.abathur.cz" ];
      # We need to override the protocol because the NextCloud instance is
      # running behind a Cloudflare tunnel and it does https for us.
      # Without this, QR codes and other JavaScript things won't work because
      # of Content-Security-Policy.
      overwriteprotocol = "https";
      overwritehost = "nextcloud.abathur.cz";
      "overwrite.cli.url" = "https://nextcloud.abathur.cz";
    };
  };

  # Nextcloud requires nginx; move it off port 80 so homepage can use it
  services.nginx = {
    enable = true;
    appendHttpConfig = ''
      types {
        text/javascript mjs;
      }
    '';
    virtualHosts."localhost" = {
      listen = [{ addr = "0.0.0.0"; port = 8080; ssl = false; }];
    };
  };
}
