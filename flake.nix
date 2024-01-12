{
    description = "Flake";

    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
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

    outputs = { self, nixpkgs, home-manager, firefox-addons, nix-index-database }@inputs:
    let
        system = "x86_64-linux";
        masterSystem = "master";
        lib = nixpkgs.lib;
        pkgs = nixpkgs.legacyPackages.${system};
    in
    {
        nixosConfigurations.${masterSystem} = lib.nixosSystem {
            system = system;
            modules = [
                ./system-${masterSystem}.nix
            ];
        };

        homeConfigurations.ramak = home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            extraSpecialArgs = { inherit inputs; inherit system; };
            modules = [
                ./home.nix
                nix-index-database.hmModules.nix-index
            ];
        };
    };
}
