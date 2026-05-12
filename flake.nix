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
    nixpkgs-acp.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs =
    { nixpkgs, home-manager, zen-browser, agenix, jail-nix, nixpkgs-acp, ... }:
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
    in
    {
      homeConfigurations."jkadlcik" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          zen-browser.homeModules.beta
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
