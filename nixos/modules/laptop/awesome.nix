{ pkgs, ... }:
{
  services.xserver = {
    windowManager.awesome = {
      enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    xclip
    xsel
    hsetroot
    xvkbd
    xcolor
    xkb-switch
  ];
}
