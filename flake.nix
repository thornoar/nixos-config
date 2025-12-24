{
  description = "NixOS Configuration Flake";

  inputs = {
    nixpkgs-unstable = { url = "github:NixOS/nixpkgs/nixos-unstable"; };
    nixpkgs = { url = "github:NixOS/nixpkgs/nixos-25.05"; };
    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
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
    nix-output-monitor = {
      url = "github:thornoar/nix-output-monitor";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    let
      system = "x86_64-linux";
      lib = inputs.nixpkgs.lib;
      firefox-pkgs = inputs.firefox-addons.packages.${system};

      readFile = file:
        (lib.lists.partition (x: 0 < lib.strings.stringLength x)
          (lib.strings.splitString "\n" (builtins.readFile file))).right;

      addition = final: prev: {
        unstable = import inputs.nixpkgs-unstable {
          inherit (final) system;
          config.allowUnfree = true;
        };
        inherit inputs;
        inherit firefox-pkgs;
        tools = {
          inherit readFile;
        };
      };

      pkgs = import inputs.nixpkgs {
        inherit system;
        config = {
          pulseaudio = true;
          allowUnfree = true;
          nvidia.acceptLicense = true;
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
                    ./home-manager/modules/scripts.nix
                    ./home-manager/modules/terminal.nix
                    ./home-manager/modules/development.nix
                  ];
                };
                extraSpecialArgs = { inherit pkgs; };
              };
            }
            inputs.nix-index-database.nixosModules.nix-index
          ];
        };

        minimal = lib.nixosSystem {
          system = system;
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
