{ config, pkgs, inputs, system, lib, pkgs-unstable, ... }:

{
    imports = (
        let
            path = /home/ramak/projects/nixos-local-config/home-local.nix;
        in if (builtins.pathExists path) then [ path ] else [ ./src/home-template.nix ]
    ) ++ [
        ./modules/options.nix
        ./modules/scripts.nix
        ./modules/external-smart.nix
        ./modules/external-direct.nix
        ./modules/development.nix
        ./modules/firefox.nix
    ];

    config = 
    let 
        # [./src/packages.txt]
        unstable-packages = with pkgs-unstable; [
            khal
            fzf
            nodejs
            # neovim
        ];
        insecure-packages = with pkgs; [
            sc-im
        ];
        custom-packages = lib.lists.forEach [
            "pshash"
            "lambda-interpreter"
        ] (x: inputs.${x}.packages.${system}.default);
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

        home.packages = (
            if config.misc.usePackageList then (
                lib.lists.forEach (
                    lib.lists.partition
                        (x: 0 < lib.strings.stringLength x) 
                        (lib.strings.splitString "\n" (builtins.readFile ./src/packages.txt))
                ).right (name: pkgs.${name})
            ) else []
		) ++ unstable-packages ++ insecure-packages ++ custom-packages;

        programs = {
            home-manager = {
                enable = true;
            };
        };

        home.pointerCursor = {
            x11.enable = true;
            gtk.enable = true;
            name = "Adwaita";
            package = pkgs.gnome.adwaita-icon-theme;
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
    };
}
