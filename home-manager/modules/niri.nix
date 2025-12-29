{ config, lib, ... }:
{
  xdg.configFile."niri/config.kdl" = config.util.dotFileMut "niri/config.kdl";
}
