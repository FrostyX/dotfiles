{ config, pkgs, ... }: {
  networking.hostName = "fizzlebang";

  age.secrets.frostyx.file = ./secrets/frostyx.age;

  users.users.frostyx = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    hashedPasswordFile = config.age.secrets.frostyx.path;
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCxZWz5K+BGQnVrirgJi+3pQhGY6+N6iiM1+u9jIOy4cC32DUnhRiUXBMfTqBQN0/0g2mG8W/Jn3Hmvzjet89Yk2rwcIzOghJ0mKLKYxbdVxgzMQRNmHmpIIis8yBfqHkNz5OO2BtxqckaQ5/xHQTwnYSLRCyNpi68ZwJUOzdvnL9y9zpH5IDJNQ6RlscLc/u6zQk9My3uPpNkwELE4brCQffHN1zc6MkDfVlVVqamWuF04YpqjoOWBe67aepUjC49eZy6uoWwxYg7IwPeZd1KyBg2y5W0dHj3OfXobv0bv4Bo/MXaMsEexJqlF4qF5A1wx2j5DIXlf4Lr8gyU9Td42C4CU2AkVjaue1e91Ay+ggotkOCEz+Wv5DKjOiTNWGS3InChbRYO2728pB6mjEh5w3I7RGF+ixtfk1w3GtVgU71uFquN3gFvVptDts/dQumsbyIl/ft0oi+FrodRH4Oz2Cb+FnDoOTMtQl5ykxzpJCDGWPr2ZXbcdIb0zFlmVnK0fjoSmpgmEwZG8Fpl2eVzxzIbM64L8+Qdr8VmTMNnmd6Z7ID8hR4qab5OBxh2IKhmashL4gZKPvsyuzfPu4sYXtIDZNBzi5sA31eTqogh1Cj6JEsUW/h+V3c7g7y3dtR3DGnPqM0H79yAlPdE5GOVq2A8OQT4snYPI+y9PrC9Hzw== jkadlcik@redhat.com"
    ];
  };

  # Open ports:
  #   22    SSH
  #   3001  Uptime Kuma
  #   6767  Bazarr
  #   7878  Radarr
  #   8082  Homepage
  #   8096  Jellyfin
  #   8384  Syncthing
  #   8686  Lidarr
  #   8080  Nextcloud (nginx)
  #   8989  Sonarr
  #   9090  Cockpit
  #   9091  Transmission
  #   9696  Prowlarr

  imports = [
    ./hardware/fizzlebang.nix
    ./modules/arr.nix
    ./modules/transmission.nix
    ./modules/nextcloud.nix
    ./modules/jellyfin.nix
    ./modules/uptime-kuma.nix
    ./modules/homepage.nix
    ./modules/cloudflared.nix
  ];

  environment.systemPackages = with pkgs; [
    htop
    iotop
    ncdu
    curl
    vim
    lsof
  ];

  console.keyMap = "cz-qwertz";

  # Configuration for when building a VM and running it locally
  virtualisation.vmVariant.virtualisation = {
    memorySize = 2048;
    forwardPorts = [
      # Connect from the host system with:
      # ssh -p 2222 frostyx@127.0.0.1
      { from = "host"; host.port = 2222; guest.port = 22; }
    ];
  };


  security.sudo.wheelNeedsPassword = false;

  services.openssh.enable = true;

  system.stateVersion = "23.11";
}
