{ config, pkgs, ... }: {
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
}
