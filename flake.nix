{
    description = "NixOS Flake";

    inputs = {
        nixpkgs = {
            url = "github:NixOS/nixpkgs/nixos-unstable";
        };
        home-manager = {
            url = "github:nix-community/home-manager";
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

    outputs = inputs:
    let
        system = "x86_64-linux";
        sysname = "master";
        usrname = "ramak";
        projects-dir = "/home/${usrname}/projects";

        pkgs = inputs.nixpkgs.legacyPackages.${system};
        hmlib = inputs.home-manager.lib;
        lib = inputs.nixpkgs.lib;
        firefox-pkgs = inputs.firefox-addons.packages.${system};
    in
    {
        nixosConfigurations.${sysname} = lib.nixosSystem {
            system = system;
            modules = [
                "${projects-dir}/nixos-config/system-${sysname}.nix"
                { _module.args = { inherit sysname; inherit usrname; inherit projects-dir; inherit inputs; }; }
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
    };
}
