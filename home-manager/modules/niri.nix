{ config, lib, ... }:
{
  xdg.configFile."niri/config.kbl" = config.util.dotFileMut "niri/config.kbl";
}
