{ pkgs, readPackages, ... }:

{
  environment.variables = {
    PCTYPE = "laptop";
    MUTTER_DEBUG_KMS_THREAD_TYPE = "user";
  };

  users.groups.extra = { };
  users.users.extra = {
    isSystemUser = true;
    description = "User for AI interface";
    group = "extra";
    extraGroups = [ "networkmanager" "sys" "wheel" ];
    homeMode = "0711";
    home = "/home/extra";
    shell = "${pkgs.zsh}/bin/zsh";
  };

  environment.systemPackages = readPackages ../../src/packages/laptop.txt pkgs;

  services = {
    libinput = {
      enable = true;
      touchpad = {
        naturalScrolling = true;
        tapping = false;
        clickMethod = "clickfinger";
        disableWhileTyping = true;
      };
    };
    syncthing = {
      enable = false;
      user = "ramak";
      dataDir = "/home/ramak/dls";
      configDir = "/home/ramak/.config/syncthing";
      settings = {
        devices = {
          "desktop" = {
            id =
              "BWFUHH5-FMJJFJO-JNDOMDV-LMIWAV2-QIJV7Y7-ZTUEPIE-V2BVDXT-QUSKLAL";
          };
        };
        folders = {
          "music" = {
            path = "~/media/music";
            ignorePerms = false;
            devices = [ ];
          };
          "books" = {
            path = "~/media/books";
            ignorePerms = false;
            devices = [ ];
          };
          "sandbox" = {
            path = "~/projects/sandbox";
            ignorePerms = false;
            devices = [ "desktop" ];
          };
        };
      };
    };
    logind.extraConfig = ''
      HandleLidSwitch=ignore
      HandleLidSwitchExternalPower=ignore
    '';
    keyd = {
      enable = true;
      keyboards.default.settings = {
        main = {
          kp2 = "down";
          kp4 = "left";
          kp6 = "right";
          kp8 = "up";
          kp1 = "end";
          kp3 = "pagedown";
          kp7 = "home";
          kp9 = "pageup";
          kpenter = "enter";
          kp5 = "down";
          rightalt = "leftmeta";
          capslock = "esc";
          esc = "capslock";
        };
      };
    };
    displayManager.autoLogin = {
      enable = false;
      user = "ramak";
    };
  };

  time.timeZone = "Europe/Belgrade";

  # May need to add nerd fonts
  fonts.packages = readPackages ../../src/packages/fonts.txt pkgs;
}
