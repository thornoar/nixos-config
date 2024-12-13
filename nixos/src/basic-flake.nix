{
    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    };

    outputs = inputs:
    let
        system = "x86_64-linux";
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        lib = inputs.nixpkgs.lib;
    in
    {
        nixosConfigurations.master = lib.nixosSystem {
            inherit system;
            modules = [ ./configuration.nix ];
        };
    };
}
