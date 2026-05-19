{
  description = "Home Manager configuration of jkadlcik";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    zen-browser = {
      url = "github:0xc000022070/zen-browser-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    jail-nix.url = "sourcehut:~alexdavid/jail.nix";
    nixible.url = "gitlab:TECHNOFAB/nixible?dir=lib";
    nix-flatpak.url = "github:gmodena/nix-flatpak/?ref=v0.7.0";
    nixpkgs-acp.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs =
    { nixpkgs, home-manager, zen-browser, agenix, jail-nix, nixible, nix-flatpak, nixpkgs-acp, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfreePredicate = pkg:
          builtins.elem (nixpkgs.lib.getName pkg) [
            "claude-code"
          ];
      };
      acpPkgs = import nixpkgs-acp {
        inherit system;
        config.allowUnfreePredicate = pkg:
          builtins.elem (nixpkgs-acp.lib.getName pkg) [
            "claude-code"
          ];
      };
      jail = jail-nix.lib.init pkgs;
      nixible_lib = nixible.lib {
        inherit (nixpkgs) lib;
        inherit pkgs;
      };
    in
    {
      packages.${system}.playbook = nixible_lib.mkNixibleCli ./playbooks/workstation.nix;

      homeConfigurations."jkadlcik" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          zen-browser.homeModules.beta
          nix-flatpak.homeManagerModules.nix-flatpak
          ./.config/home-manager/home.nix
        ];
        extraSpecialArgs = {
          hostname = "hive";
          inherit jail acpPkgs;
        };
      };

      homeConfigurations."jkadlcik@pop-os" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          zen-browser.homeModules.beta
          nix-flatpak.homeManagerModules.nix-flatpak
          ./.config/home-manager/home.nix
        ];
        extraSpecialArgs = {
          hostname = "pop-os";
          inherit jail acpPkgs;
        };
      };

      homeConfigurations."jkadlcik@nova" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          zen-browser.homeModules.beta
          nix-flatpak.homeManagerModules.nix-flatpak
          ./.config/home-manager/home.nix
        ];
        extraSpecialArgs = {
          hostname = "nova";
          inherit jail acpPkgs;
        };
      };

      nixosConfigurations.fizzlebang = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          agenix.nixosModules.default
          ./nixos/fizzlebang.nix
        ];
      };
    };
}
