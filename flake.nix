{
    description = "NixOS Flake";

    inputs = {
        nixpkgs = {
            url = "github:NixOS/nixpkgs/nixos-unstable";
        };
        home-manager = {
            url = "github:nix-community/home-manager";
            # url = "github:nix-community/home-manager/release-23.11";
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
        usrname = "ramak";
        usrnames = [ usrname ];
        projects-dir = /home/${usrname}/projects;

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
    in
    {
        nixosConfigurations = listToAttrset sysnames (sysname:
            lib.nixosSystem {
                system = system;
                modules = [
                    "${projects-dir}/nixos-config/system-${sysname}.nix"
                    { _module.args = { inherit sysname; inherit usrname; inherit projects-dir; inherit inputs; }; }
                ];
            }
        );

        homeConfigurations."${usrname}" = hmlib.homeManagerConfiguration {
            inherit pkgs;
            extraSpecialArgs = { inherit inputs; inherit system; inherit usrname; };
            modules = [
                "${projects-dir}/nixos-config/home-${usrname}/main.nix"
                # ./${usrname}/main.nix
                inputs.nix-index-database.hmModules.nix-index
            ];
        };
    };
}
