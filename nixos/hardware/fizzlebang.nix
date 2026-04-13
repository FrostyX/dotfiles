{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];

  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/vda";

  boot.initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "virtio_pci" "sr_mod" "virtio_blk" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/2350e6ee-60e6-44fa-bace-3682c57ea6a3";
    fsType = "ext4";
  };

  fileSystems."/mnt/data1" = {
    device = "192.100.2.1:/volume1/data1";
    fsType = "nfs";
    options = [ "nfsvers=3" "soft" "x-systemd.automount" "noauto" ];
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/6c8ba969-c81f-4889-a8e2-fd9013d5f18c"; }
  ];

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
