{ pkgs, readPackages, ... }:

{
  environment.variables = {
    PCTYPE = "laptop";
    MUTTER_DEBUG_KMS_THREAD_TYPE = "user";
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
      enable = true;
      user = "ramak";
    };
  };

  time.timeZone = "Europe/Belgrade";

  fonts.packages = readPackages ../../src/packages/fonts.txt pkgs;
}
