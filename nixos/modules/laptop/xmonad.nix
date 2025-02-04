{ pkgs, pkgs-unstable, lib, readPackages, ... }:
{
    specialisation.xmonad.configuration = {
        boot.loader.systemd-boot.sortKey = "aab";
        environment.variables = {
            SPECIALISATION = lib.mkForce "xmonad";
            TERMINAL = "kitty";
            XCURSOR_SIZE = "16";
            BROWSER = "firefox -P xmonad";
        };
        services.xserver.dpi = 192;
        services.xserver = {
            enable = true;
            xkb.layout = "us";
            xkb.variant = "";
            windowManager.xmonad = {
                enable = true;
                enableContribAndExtras = true;
                haskellPackages = pkgs-unstable.haskellPackages;
            };
            displayManager = {
                sessionCommands = ''
                    # nvidia-settings --assign CurrentMetaMode="nvidia-auto-select +0+0 { ForceFullCompositionPipeline = On }"
                    xrandr --output eDP-1 --scale 1.0x1.0
                    setxkbmap -layout us,ru,de
                    xset -dpms
                    setterm -blank 0 -powerdown 0
                    xset r rate 200 30
                    xset s off
                    transmission-daemon
                    setxkbmap -option grp:alt_caps_toggle us,ru
                '';
            };
        };
        services = {
            picom = {
                enable = true;
                backend = "glx";
            };
            unclutter-xfixes = {
                enable = true;
                timeout = 1;
            };
        };
        environment.systemPackages = readPackages ../../src/packages/xmonad.txt pkgs;
    };
}
