{ pkgs, ... }:

{
  environment.variables = {
    PCTYPE = "laptop";
    MUTTER_DEBUG_KMS_THREAD_TYPE = "user";
  };

  environment.systemPackages = pkgs.tools.readPackages ../../src/packages/laptop.txt pkgs;

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
    ollama = {
      package = pkgs.unstable.ollama;
      enable = true;
      acceleration = "cuda";
    };
  };

  programs = {
    steam = {
      enable = true;
      remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
      dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
      localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
    };
    gamemode.enable = true;
  };

  time.timeZone = "Asia/Hong_Kong";

  fonts.packages = pkgs.tools.readPackages ../../src/packages/fonts.txt pkgs;
}
