{
    description = "NixOS Flake";

    inputs = {
        nixpkgs = {
            # url = "github:NixOS/nixpkgs/nixos-unstable";
            url = "github:NixOS/nixpkgs/nixos-23.11";
        };
        home-manager = {
            # url = "github:nix-community/home-manager";
            url = "github:nix-community/home-manager/release-23.11";
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
    };

    outputs = inputs @ { self, ... }:
    let
        system = "x86_64-linux";
        sysname = "master";
        usrname = "ramak";

        pkgs = inputs.nixpkgs.legacyPackages.${system};
        hmlib = inputs.home-manager.lib;
        lib = inputs.nixpkgs.lib;
        firefox-pkgs = inputs.firefox-addons.packages.${system};
    in
    {
        nixosConfigurations.${sysname} = lib.nixosSystem {
            system = system;
            modules = [
                ./system-${sysname}.nix
                { _module.args = { inherit sysname; inherit usrname; inherit inputs; }; }
                inputs.home-manager.nixosModules.home-manager
                {
                    home-manager = {
                        useGlobalPkgs = true;
                        useUserPackages = true;
                        users.${usrname} = import ./home-${usrname}/main.nix;
                        extraSpecialArgs = { inherit usrname; inherit firefox-pkgs; };
                    };
                }
                inputs.nix-index-database.nixosModules.nix-index
            ];
        };

        packages.${system} = {
            install = pkgs.writeShellApplication {
                name = "install";
                runtimeInputs = with pkgs; [ git ];
                text = ''${./install.sh} "$@"'';
            };
            default = self.packages.${system}.install;
        };

        apps.${system} = {
            install = {
                type = "app";
                program = "${self.packages.${system}.install}/bin/install";
            };
            default = self.apps.${system}.install;
        };
    };
}
