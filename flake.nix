{
    description = "NixOS Flake";

    inputs = {
        nixpkgs = {
            url = "github:NixOS/nixpkgs/nixos-24.05";
        };
        nixpkgs-unstable = {
            url = "github:NixOS/nixpkgs/nixos-unstable";
        };
        home-manager = {
            url = "github:nix-community/home-manager/release-24.05";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        firefox-addons = {
            url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        nix-index-database = {
            url = "github:Mic92/nix-index-database";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    };

    outputs = inputs @ { self, ... }:
    let
        system = "x86_64-linux";
        sysname = "master";

        pkgs = inputs.nixpkgs.legacyPackages.${system};
        pkgs-unstable = inputs.nixpkgs-unstable.legacyPackages.${system};
        lib = inputs.nixpkgs.lib;
        firefox-pkgs = inputs.firefox-addons.packages.${system};
    in
    {
        nixosConfigurations = {
            ${sysname} = lib.nixosSystem {
                system = system;
                modules = [
                    ./system-${sysname}.nix
                    { _module.args = { inherit sysname; inherit inputs; inherit pkgs-unstable; }; }
                    inputs.home-manager.nixosModules.home-manager
                    {
                        home-manager = {
                            useUserPackages = true;
                            users.ramak = import ./home-manager/main.nix;
                            extraSpecialArgs = { inherit firefox-pkgs; inherit pkgs-unstable; };
                        };
                    }
                    inputs.nix-index-database.nixosModules.nix-index
                ];
            };

            minimal = lib.nixosSystem {
                system = system;
                modules = [
                    ./system-minimal.nix
                    { _module.args = { inherit sysname; inherit inputs; inherit pkgs-unstable; }; }
                ];
            };

            isoimage = lib.nixosSystem {
                specialArgs = { inherit inputs; };
                modules = [
                    ./isoimage/configuration.nix
                    # inputs.nixos-hardware.nixosModules.asus-pro-ws-x570-ace
                ];
            };
        };
    };
}
