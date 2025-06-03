{
  description = "NixOS Configuration Flake";

  inputs = {
    nixpkgs-unstable = { url = "github:NixOS/nixpkgs/nixos-unstable"; };
    nixpkgs = { url = "github:NixOS/nixpkgs/nixos-24.11"; };
    nixpkgs-old = { url = "github:NixOS/nixpkgs/nixos-23.11"; };
    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    firefox-addons = {
      url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur = {
      url = "github:nix-community/NUR";
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
      readFile = file:
        (lib.lists.partition (x: 0 < lib.strings.stringLength x)
          (lib.strings.splitString "\n" (builtins.readFile file))).right;
      getAttr = set: name:
        let
          split = lib.strings.splitString "." name;
          str = lib.strings;
        in if (1 == builtins.length split) then
          set.${name}
        else
          getAttr (set.${builtins.head split})
          (str.concatStrings (str.intersperse "." (builtins.tail split)));
      readPackages = file: set: lib.lists.forEach (readFile file) (getAttr set);
    in {
      nixosConfigurations = {
        laptop = lib.nixosSystem {
          inherit system;
          modules = [
            ./nixos/configuration.nix
            ./nixos/modules/laptop/configuration.nix
            ./nixos/modules/laptop/hardware-configuration.nix
            ./nixos/modules/laptop/hardware-manual.nix
            ./nixos/modules/laptop/hyprland.nix
            ./nixos/modules/laptop/hyprland-powersave.nix
            ./nixos/modules/laptop/xmonad.nix
            ./nixos/modules/vpn.nix
            {
              _module.args = {
                sysname = "laptop";
                inherit system;
                inherit inputs;
                inherit pkgs-unstable;
                inherit pkgs-old;
                inherit readPackages;
              };
            }
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
                    ./home-manager/modules/dispatch.nix
                    ./home-manager/modules/hyprland.nix
                    ./home-manager/modules/xmonad.nix
                    ./home-manager/modules/terminal.nix
                    ./home-manager/modules/development.nix
                    ./home-manager/modules/firefox.nix
                  ];
                };
                extraSpecialArgs = {
                  inherit inputs;
                  inherit system;
                  inherit firefox-pkgs;
                  inherit pkgs-unstable;
                  inherit readFile;
                  inherit readPackages;
                };
              };
            }
            inputs.nix-index-database.nixosModules.nix-index
          ];
        };

        desktop = lib.nixosSystem {
          inherit system;
          modules = [
            ./nixos/configuration.nix
            ./nixos/modules/desktop/configuration.nix
            ./nixos/modules/desktop/hardware-configuration.nix
            {
              _module.args = {
                sysname = "desktop";
                inherit inputs;
                inherit pkgs-unstable;
                inherit pkgs-old;
              };
            }
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
                    ./home-manager/modules/terminal.nix
                    ./home-manager/modules/development.nix
                  ];
                };
                extraSpecialArgs = {
                  inherit inputs;
                  inherit system;
                  inherit firefox-pkgs;
                  inherit pkgs-unstable;
                  inherit readFile;
                  inherit readPackages;
                };
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
                inherit pkgs-unstable;
              };
            }
          ];
        };

        isoimage = lib.nixosSystem {
          specialArgs = { inherit inputs; };
          modules = [ ./isoimage/configuration.nix ];
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
