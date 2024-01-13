{
    description = "Flake";

    inputs = {
        nixpkgs = {
            url = "github:NixOS/nixpkgs/nixos-unstable";
        };
        home-manager = {
            url = "github:nix-community/home-manager";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        flake-utils = {
            url = "github:numtide/flake-utils";
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
        sysnames = [ "master" ];
        usrnames = [ "ramak" ];

        pkgs = inputs.nixpkgs.legacyPackages.${system};
        hmlib = inputs.home-manager.lib;
        lib = inputs.nixpkgs.lib;
        listToAttrset = lst: fn: lib.attrsets.mergeAttrsList
        (
            lib.lists.zipListsWith
                (str: val: { ${str} = val; })
                lst
                (lib.lists.forEach lst fn)
        );
        usrs = listToAttrset usrnames (name: {
            isNormalUser = true;
            description = name;
            extraGroups = [ "networkmanager" "wheel" "sys" "root" "audio" "sound" "video" "networkmanager" ];
            packages = with pkgs; [];
        });
    in
    {
        nixosConfigurations = listToAttrset sysnames (sysname:
            lib.nixosSystem {
                system = system;
                modules = [
                    ./system-${sysname}.nix
                    { _module.args = { inherit sysname; inherit usrs; }; }
                ];
            }
        );

        homeConfigurations = listToAttrset usrnames (usrname: 
            hmlib.homeManagerConfiguration {
                inherit pkgs;
                extraSpecialArgs = { inherit inputs; inherit system; inherit usrname; };
                modules = [
                    ./home-${usrname}.nix
                    inputs.nix-index-database.hmModules.nix-index
                ];
            }
        );

        # nixosConfigurations.${masterSystem} = lib.nixosSystem {
        #     system = system;
        #     modules = [
        #         ./system-${masterSystem}.nix
        #     ];
        # };

        # homeConfigurations.ramak = inputs.home-manager.lib.homeManagerConfiguration {
        #     inherit pkgs;
        #     extraSpecialArgs = { inherit inputs; inherit system; };
        #     modules = [
        #         ./home.nix
        #         inputs.nix-index-database.hmModules.nix-index
        #     ];
        # };
    };
}
