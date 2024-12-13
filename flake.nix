{
    description = "NixOS Flake";

    inputs = {
        nixpkgs-unstable = {
            url = "github:NixOS/nixpkgs/nixos-unstable";
        };
        nixpkgs = {
            url = "github:NixOS/nixpkgs/nixos-24.11";
        };
        nixpkgs-old = {
            url = "github:NixOS/nixpkgs/nixos-23.11";
        };
        home-manager = {
            url = "github:nix-community/home-manager/release-24.11";
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
        pshash = {
            url = "github:thornoar/pshash/master";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        lambda-interpreter = {
            url = "github:thornoar/lambda-interpreter";
            inputs.nixpkgs.follows = "nixpkgs";
        };
    };

    outputs = inputs:
    let
        system = "x86_64-linux";
        sysname = "master";

        pkgs-unstable = import inputs.nixpkgs-unstable {
            inherit system;
            config.allowUnfree = true;
        };
        pkgs = import inputs.nixpkgs {
            inherit system;
            config.allowUnfree = true;
        };
        pkgs-old = import inputs.nixpkgs-old {
            inherit system;
            config.allowUnfree = true;
        };
        lib = inputs.nixpkgs.lib;
        firefox-pkgs = inputs.firefox-addons.packages.${system};
    in
    {
        nixosConfigurations = {
            ${sysname} = lib.nixosSystem {
                inherit system;
                modules = [
                    ./nixos/configuration.nix
                    ./nixos/modules/${sysname}.nix
                    { _module.args = { inherit sysname; inherit inputs; inherit pkgs-unstable; inherit pkgs-old; }; }
                    inputs.home-manager.nixosModules.home-manager
                    {
                        home-manager = {
                            useUserPackages = true;
                            users.ramak = import ./home-manager/home.nix;
                            extraSpecialArgs = { inherit inputs; inherit system; inherit firefox-pkgs; inherit pkgs-unstable; };
                        };
                    }
                    inputs.nix-index-database.nixosModules.nix-index
                ];
            };

            minimal = lib.nixosSystem {
                system = system;
                modules = [
                    ./nixos/configuration.nix
                    { _module.args = { inherit sysname; inherit inputs; inherit pkgs-unstable; }; }
                ];
            };

            isoimage = lib.nixosSystem {
                specialArgs = { inherit inputs; };
                modules = [
                    ./isoimage/configuration.nix
                ];
            };
        };

        # homeConfigurations = {
        #     ramak = inputs.home-manager.lib.homeManagerConfiguration {
        #         inherit pkgs;
        #         modules = [
        #             ./home-manager/home.nix
        #             inputs.home-manager-diff.hmModules.default
        #             # {
        #             #     home.packages = [
        #             #         inputs.pshash.packages.${system}.default
        #             #         inputs.lambda-interpreter.packages.${system}.default
        #             #     ];
        #             # }
        #         ];
        #         extraSpecialArgs = { inherit inputs; inherit system; inherit firefox-pkgs; inherit pkgs-unstable; inherit pkgs-old; };
        #     };
        # };
    };
}
