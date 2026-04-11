{ config, pkgs, ... }: {
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
}
