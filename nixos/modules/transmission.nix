{ config, pkgs, ... }: {
  age.secrets.transmission.file = ../secrets/transmission.age;

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
    credentialsFile = config.age.secrets.transmission.path;
  };
}
