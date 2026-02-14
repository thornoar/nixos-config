{
  description = "NixOS Configuration Flake";

  inputs = {
    nixpkgs-unstable = { url = "github:NixOS/nixpkgs/nixos-unstable"; };
    nixpkgs = { url = "github:NixOS/nixpkgs/nixos-25.11"; };
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
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
      lib = inputs.nixpkgs.lib;

      addition = final: prev: {
        unstable = import inputs.nixpkgs-unstable {
          system = final.stdenv.hostPlatform.system;
          config.allowUnfree = true;
        };
        inherit inputs;
        firefox-pkgs = inputs.firefox-addons.packages.${system};
      };

      pkgs = import inputs.nixpkgs {
        inherit system;
        config = {
          # pulseaudio = true;
          allowUnfree = true;
          # android_sdk.accept_license = true;
          # nvidia.acceptLicense = true;
        };
        overlays = [ addition ];
      };
    in {
      nixosConfigurations = {
        laptop = lib.nixosSystem {
          inherit system;
          inherit pkgs;
          modules = [
            ./nixos/configuration.nix
            ./nixos/modules/laptop/configuration.nix
            ./nixos/modules/laptop/hardware-configuration.nix
            ./nixos/modules/laptop/hardware-manual.nix
            ./nixos/modules/laptop/hyprland.nix
            # ./nixos/modules/laptop/niri.nix
            { _module.args = { sysname = "laptop"; }; }
            inputs.home-manager.nixosModules.home-manager
            {
              home-manager = {
                useUserPackages = true;
                users.ramak = { ... }: {
                  imports = [
                    ./home-manager/home.nix
                    ./home-manager/modules/options/declaration.nix
                    ./home-manager/modules/options/laptop.nix
                    ./home-manager/modules/laptop.nix
                    ./home-manager/modules/hyprland.nix
                    # ./home-manager/modules/niri.nix
                    ./home-manager/modules/firefox.nix
                    ./home-manager/modules/development.nix
                  ];
                };
                extraSpecialArgs = { inherit pkgs; };
              };
            }
            inputs.nix-index-database.nixosModules.nix-index
          ];
        };

        desktop = lib.nixosSystem {
          inherit system;
          inherit pkgs;
          modules = [
            ./nixos/configuration.nix
            ./nixos/modules/desktop/configuration.nix
            ./nixos/modules/desktop/hardware-configuration.nix
            { _module.args = { sysname = "desktop"; }; }
            inputs.home-manager.nixosModules.home-manager
            {
              home-manager = {
                useUserPackages = true;
                users.ramak = { ... }: {
                  imports = [
                    ./home-manager/home.nix
                    ./home-manager/modules/desktop.nix
                    ./home-manager/modules/options/declaration.nix
                    ./home-manager/modules/options/desktop.nix
                  ];
                };
                extraSpecialArgs = { inherit pkgs; };
              };
            }
            inputs.nix-index-database.nixosModules.nix-index
          ];
        };

        minimal = lib.nixosSystem {
          inherit system;
          modules = [
            ./nixos/configuration.nix
            {
              _module.args = {
                sysname = "minimal";
                inherit inputs;
              };
            }
          ];
        };

        isoimage = lib.nixosSystem {
          specialArgs = { inherit inputs; };
          modules = [ ./isoimage/configuration.nix ];
        };
      };
    };
}
