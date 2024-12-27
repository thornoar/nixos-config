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
            laptop = lib.nixosSystem {
                inherit system;
                modules = [
                    ./nixos/configuration.nix
                    # ./nixos/modules/hardware-configuration/laptop.nix
                    ./nixos/modules/laptop/configuration.nix
                    ./nixos/modules/laptop/hardware-configuration.nix
                    ./nixos/modules/vpn.nix
                    ./nixos/modules/window-managers.nix
                    { _module.args = { sysname = "laptop"; inherit inputs; inherit pkgs-unstable; inherit pkgs-old; }; }
                    inputs.home-manager.nixosModules.home-manager
                    {
                        home-manager = {
                            useUserPackages = true;
                            users.ramak = { ... }: {
                                imports = [
                                    ./home-manager/home.nix
                                    ./home-manager/modules/options/declaration.nix
                                    ./home-manager/modules/options/laptop.nix
                                    ./home-manager/modules/scripts.nix
                                    ./home-manager/modules/external-tui.nix
                                    ./home-manager/modules/external-gui.nix
                                    ./home-manager/modules/development.nix
                                    ./home-manager/modules/firefox.nix
                                ];
                            };
                            extraSpecialArgs = { inherit inputs; inherit system; inherit firefox-pkgs; inherit pkgs-unstable; };
                        };
                    }
                    inputs.nix-index-database.nixosModules.nix-index
                ];
            };

            desktop = lib.nixosSystem {
                inherit system;
                modules = [
                    ./nixos/configuration.nix
                    { _module.args = { sysname = "desktop"; inherit inputs; inherit pkgs-unstable; inherit pkgs-old; }; }
                    inputs.home-manager.nixosModules.home-manager
                    {
                        home-manager = {
                            useUserPackages = true;
                            users.ramak = { ... }: {
                                imports = [
                                    ./home-manager/home.nix
                                    ./home-manager/modules/options/declaration.nix
                                    ./home-manager/modules/options/desktop.nix
                                    ./home-manager/modules/scripts.nix
                                    ./home-manager/modules/external-tui.nix
                                    ./home-manager/modules/development.nix
                                ];
                            };
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
                    { _module.args = { sysname = "minimal"; inherit inputs; inherit pkgs-unstable; }; }
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
