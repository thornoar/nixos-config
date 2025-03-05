{ config, lib, pkgs, modulesPath, ... }:

{
  options = {

  };
  config = {
    environment.variables = { PCTYPE = "---"; };

    fileSystems."/path/to/dir" = {
      device = "/dev/---";
      fsType = "ext4";
      options = [ "nofail" "rw" "user" "auto" ];
    };
  };
}
