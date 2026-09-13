{ lib, hostname, ... }:

lib.mkIf (hostname == "nova") {
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;

    settings."github.com" = {
      AddKeysToAgent = "yes";
      IdentityFile = "~/.ssh/id_ed25519_sk_rk";
    };
  };
}
