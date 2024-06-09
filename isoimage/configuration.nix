{ pkgs, modulesPath, ... }:
{
    imports = [
        "${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix"
    ];

    config = {
        # boot.extraModulePackages = [ config.boot.kernelPackages.rtl88x2bu ];

        nixpkgs.config.allowUnfree = true;

        hardware.opengl = {
            enable = true;
            driSupport = true;
            driSupport32Bit = true;
        };

        nixpkgs.hostPlatform = "x86_64-linux";

        environment.systemPackages = with pkgs; [
            vim
            parted
            git
            wget
        ];

        nix.settings.experimental-features = [ "nix-command" "flakes" ];
        
        services.xserver = {
            videoDrivers = [ "nvidia" ];
            enable = true;
            displayManager.sddm.enable = true;
            desktopManager.plasma5.enable = true;
        };
    };
}
