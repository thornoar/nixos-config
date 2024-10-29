{ pkgs, modulesPath, ... }:

{
    imports = [
        "${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix"
    ];

    config = {
        # boot.extraModulePackages = [ config.boot.kernelPackages.rtl88x2bu ];
        # boot.kernelPackages = pkgs.linuxKernel.packageAliases.linux_latest;

        boot.initrd.kernelModules = [ "xhci_hcd" "ehci_pci" "ahci" "hid_microsoft" "hid_generic" "dm_mod" "atkbd" ];
        # boot.initrd.availableKernelModules = [
        # "md_mod" "raid0" "raid1" "raid456" "ext2" "ext3" "ext4" "dm_mod"
        # "dm_crypt" "cryptd" "aes" "aes_generic" "blowfish" "twofish"
        # "serpent" "cbc" "xts" "lrw"
        # "ahci" "sata_nv" "sata_via" "sata_sis" "sata_uli" "ata_piix"
        # "pata_marvell" "sd_mod" "sr_mod" "ide_disk" "ide_generic"
        # "uhci_hcd" "ehci_hcd" "ehci_pci" "ohci_hcd" "xhci_hcd" "usbhid"
        # "hid_generic" "unix" "pcips2" "xtkbd" "scsi_wait_scan"
        # ];

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

        services = {
            xserver = {
                videoDrivers = [ "nvidia" ];
                enable = true;
                # desktopManager.plasma5.enable = true;
            };
            # displayManager.sddm.enable = true;
        };
    };
}
