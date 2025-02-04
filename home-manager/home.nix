{ config, pkgs, inputs, system, lib, pkgs-unstable, readFile, readPackages, ... }:

let
    readCustomPackages = file: lib.lists.forEach (readFile file) (x: inputs.${x}.packages.${system}.default);
in
{
    home.username = "ramak";
    home.homeDirectory = "/home/ramak";
    xdg.userDirs = {
        enable = true;
        download = "${config.home.homeDirectory}/dls";
        desktop = "${config.home.homeDirectory}/dsk";
        documents = "${config.home.homeDirectory}/docs";
        pictures = "${config.home.homeDirectory}/media/pictures";
    };
    home.sessionVariables = {
        BAT_THEME = "base16";
        WALLPAPER_DIR = config.wallpaper.dir;
    };

    xdg.mimeApps = rec {
        enable = true;
        associations.added = {
            "application/pdf" = [ "org.pwmt.zathura-pdf-mupdf.desktop" ];
            "audio/mpeg" = [ "mpv.desktop" ];
            "audio/mp3" = [ "mpv.desktop" ];
            "video/vnd.avi" = [ "mpv.desktop" ];
            "image/vnd.djvu+multipage" = [ "org.pwmt.zathura-pdf-mupdf.desktop" ];
            "image/svg+xml" = [ "imv.desktop" ];
            "text/csv" = [ "sc-im.desktop" ];
        };
        defaultApplications = associations.added;
    };
    nixpkgs.config.allowUnfree = true;

    home.packages =
        readPackages ./src/packages/general.txt pkgs ++
        readPackages ./src/packages/unstable.txt pkgs-unstable ++
        readPackages ./src/packages/insecure.txt pkgs ++
        readCustomPackages ./src/packages/custom.txt;

    # (
    #     if config.misc.usePackageList then (
    #         lib.lists.forEach (
    #             lib.lists.partition
    #                 (x: 0 < lib.strings.stringLength x) 
    #                 (lib.strings.splitString "\n" (builtins.readFile ./src/packages/general.txt))
    #         ).right (name: pkgs.${name})
    #     ) else []
    # ) ++ unstable-packages ++ insecure-packages ++ custom-packages;

    programs = {
        home-manager = {
            enable = true;
        };
    };

    home.pointerCursor = {
        x11.enable = true;
        gtk.enable = true;
        name = "Adwaita";
        package = pkgs.adwaita-icon-theme;
        # package = pkgs.rose-pine-cursor;
    };

    dconf.settings = {
        "org/virt-manager/virt-manager/connections" = {
            autoconnect = ["qemu:///system"];
            uris = ["qemu:///system"];
        };
    };

    gtk = {
        enable = true;
        font.name = "Hack Mono 11";
        theme = {
            name = "deepin-dark";
            package = pkgs.deepin.deepin-gtk-theme;
        };
    };

    # Keynav service
    services.keynav.enable = true;

    # Enabling insecure library
    nixpkgs.config = {
        permittedInsecurePackages = [
            "libxls-1.6.2"
        ];
    };

    home.stateVersion = "23.11";
}
