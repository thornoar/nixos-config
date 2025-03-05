{ pkgs, modulesPath, ... }:

{
  imports = [ "${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix" ];

  config = {
    # boot.extraModulePackages = [ config.boot.kernelPackages.rtl88x2bu ];
    # boot.kernelPackages = pkgs.linuxKernel.packageAliases.linux_latest;

    boot.initrd.kernelModules = [
      "xhci_hcd"
      "ehci_pci"
      "ahci"
      "hid_microsoft"
      "hid_generic"
      "dm_mod"
      "atkbd"
    ];

    nixpkgs.config.allowUnfree = true;

    nixpkgs.hostPlatform = "x86_64-linux";

    environment.systemPackages = with pkgs; [
      vim
      parted
      git
      wget
      curl
      neovim
      tmux
    ];

    nix.settings.experimental-features = [ "nix-command" "flakes" ];
  };
}
