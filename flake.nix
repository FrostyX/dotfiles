{
  description = "Home Manager configuration of jkadlcik";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      homeConfigurations."jkadlcik" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ ./.config/home-manager/home.nix ];
        extraSpecialArgs = {
          hostname = "hive";
        };
      };

      homeConfigurations."jkadlcik@pop-os" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ ./.config/home-manager/home.nix ];
        extraSpecialArgs = {
          hostname = "pop-os";
        };
      };
    };
}
