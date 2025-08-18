{ pkgs, ... }: {
  boot = {
    blacklistedKernelModules = [
      "nouveau" # "nvidia" "nvidia_drm" "nvidia_modeset"
    ];
    kernelPackages = pkgs.linuxPackages_6_12;
    loader.systemd-boot = { configurationLimit = 2; };
  };

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };
  powerManagement = {
    enable = true;
    powertop.enable = true;
    # cpuFreqGovernor = "powersave";
    # resumeCommands = "${pkgs.kmod}/bin/rmmod atkbd; ${pkgs.kmod}/bin/modprobe atkbd reset=1";
  };

  fileSystems."/home/ramak/media" = {
    device = "/dev/disk/by-uuid/aa543ce3-5cbd-4251-a01c-59ebe4a97f92";
    fsType = "ext4";
    options = [ "nofail" "rw" "user" "auto" "exec" ];
  };
  fileSystems."/home/ramak/media/films" = {
    device = "/dev/disk/by-uuid/d365c266-1fdd-42b1-a576-e7e9efd3e53f";
    fsType = "ext4";
    options = [ "nofail" "rw" "user" "auto" ];
  };

  services = {
    cron = {
      enable = true;
      systemCronJobs = [
        "*/1 * * * * root ${pkgs.coreutils}/bin/echo disable > /sys/firmware/acpi/interrupts/sci"
        "*/1 * * * * root ${pkgs.coreutils}/bin/echo disable > /sys/firmware/acpi/interrupts/gpe6F"
      ];
    };
    upower.enable = true;
    thermald.enable = true;
    auto-cpufreq = {
      enable = true;
      settings = {
        battery = {
          governor = "powersave";
          turbo = "never";
        };
        charger = {
          governor = "performance";
          turbo = "auto";
        };
      };
    };
  };

  systemd.timers."interrupt-allow-access" = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "1";
      OnUnitActiveSec = "10m";
      Unit = "interrupt-allow-access.service";
    };
  };
  systemd.services."interrupt-allow-access" = {
    script = ''
      chmod 777 /sys/firmware/acpi/interrupts/sci
      chmod 777 /sys/firmware/acpi/interrupts/gpe6F
    '';
    serviceConfig = {
      Type = "oneshot";
      User = "root";
    };
    wantedBy = [ "multi-user.target" ];
  };

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    settings = { General.Experimental = true; };
  };
}
