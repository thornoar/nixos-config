{ pkgs, ... }:

{
    imports = [
        ./vpn.nix
        ./specialisation.nix
        ./services.nix
    ] ++ (
        let
            path = /home/ramak/projects/nixos-local-config/system-local.nix;
        in
            if (builtins.pathExists path) then [ path ] else [ ../src/system-template.nix ]
    );

    config = {
        environment = {
            variables.SPECIALISATION = "default";
            wordlist.enable = true;
            systemPackages = with pkgs; [
                scowl
                manix
            ];
        };

        fonts.packages = with pkgs; [
            hack-font
            noto-fonts
            kochi-substitute
            nerdfonts
        ];

        services.pipewire.enable = false;
        hardware.pulseaudio.enable = true;
        hardware.pulseaudio.support32Bit = true;
        nixpkgs.config = {
            pulseaudio = true;
            allowUnfree = true;
        };

        virtualisation.libvirtd.enable = true;
        programs = {
            virt-manager.enable = true;
        };
    };
}
