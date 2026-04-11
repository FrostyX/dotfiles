{ config, pkgs, ... }: {
  age.secrets.nextcloud.file = ../secrets/nextcloud.age;

  services.nextcloud = {
    enable = true;
    package = pkgs.nextcloud32;
    hostName = "localhost";
    home = "/mnt/data2/nextcloud";
    https = false;
    database.createLocally = true;
    config = {
      dbtype = "pgsql";
      adminpassFile = config.age.secrets.nextcloud.path;
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
