{ pkgs, lib, ... }:
{
  boot = {
    blacklistedKernelModules = [
      "nouveau" # "nvidia" "nvidia_drm" "nvidia_modeset"
    ];
    kernelPackages = pkgs.linuxPackages_6_18;
    loader = {
      systemd-boot = {
        enable = true;
        configurationLimit = 10;
      };
      efi.canTouchEfiVariables = true;
      timeout = 3;
    };
    extraModprobeConfig = lib.mkDefault ''
      blacklist nouveau
      options nouveau modeset=0
    '';
    supportedFilesystems = [ "ntfs" ];
    tmp.cleanOnBoot = true;
  };
  # systemd.extraConfig = ''
  #   DefaultTimeoutStopSec=3s
  # '';

  powerManagement = {
    enable = true;
    powertop.enable = true;
    cpuFreqGovernor = "powersave";
  };

  fileSystems."/home/ramak/media" = {
    device = "/dev/disk/by-uuid/aa543ce3-5cbd-4251-a01c-59ebe4a97f92";
    fsType = "ext4";
    options = [ "nofail" "rw" "user" "auto" "exec" ];
  };
  fileSystems."/home/ramak/media/films" = {
    device = "/dev/disk/by-uuid/d365c266-1fdd-42b1-a576-e7e9efd3e53f";
    fsType = "ext4";
    options = [ "nofail" "rw" "user" "auto" "exec" ];
  };

  services = {
    # cron = {
    #   enable = true;
    #   systemCronJobs = [
    #     "*/1 * * * * root ${pkgs.coreutils}/bin/echo disable > /sys/firmware/acpi/interrupts/sci"
    #     "*/1 * * * * root ${pkgs.coreutils}/bin/echo disable > /sys/firmware/acpi/interrupts/gpe6F"
    #   ];
    # };
    upower.enable = true;
    thermald.enable = true;
    # auto-cpufreq = {
    #   enable = true;
    #   settings = {
    #     battery = {
    #       governor = "powersave";
    #       turbo = "never";
    #     };
    #     charger = {
    #       governor = "performance";
    #       turbo = "auto";
    #     };
    #   };
    # };
    tlp = {
      enable = true;
      settings = {
        CPU_SCALING_GOVERNOR_ON_AC = "performance";
        CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

        CPU_ENERGY_PERF_POLICY_ON_AC = "performance";
        CPU_ENERGY_PERF_POLICY_ON_BAT = "power";

        CPU_MIN_PERF_ON_AC = 0;
        CPU_MAX_PERF_ON_AC = 100;
        CPU_MIN_PERF_ON_BAT = 0;
        CPU_MAX_PERF_ON_BAT = 20;

        START_CHARGE_THRESH_BAT0 = 40; # 40 and below it starts to charge
        STOP_CHARGE_THRESH_BAT0 = 80; # 80 and above it stops charging
      };
    };
  };

  systemd.timers."interrupt-silence" = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "0";
      OnUnitActiveSec = "15s";
      Unit = "interrupt-silence.service";
    };
  };
  systemd.services."interrupt-silence" = {
    script = ''
      chmod 777 /sys/firmware/acpi/interrupts/sci
      chmod 777 /sys/firmware/acpi/interrupts/gpe6F
      ${pkgs.coreutils}/bin/echo mask > /sys/firmware/acpi/interrupts/sci || ${pkgs.coreutils}/bin/echo -n ""
      ${pkgs.coreutils}/bin/echo mask > /sys/firmware/acpi/interrupts/gpe6F || ${pkgs.coreutils}/bin/echo -n ""
      ${pkgs.coreutils}/bin/echo disable > /sys/firmware/acpi/interrupts/sci || ${pkgs.coreutils}/bin/echo -n ""
      ${pkgs.coreutils}/bin/echo disable > /sys/firmware/acpi/interrupts/gpe6F || ${pkgs.coreutils}/bin/echo -n ""
    '';
    serviceConfig = {
      Type = "oneshot";
      User = "root";
    };
    wantedBy = [ "multi-user.target" ];
  };

  hardware = {
    bluetooth = {
      enable = true;
      powerOnBoot = true;
      settings = { General.Experimental = true; };
    };

    graphics = {
      enable = true;
      enable32Bit = true;
      extraPackages = with pkgs; [ intel-compute-runtime ];
    };

    nvidia = {
      modesetting.enable = true;
      open = false;
      prime = {
        # offload = {
        #   enable = true;
        #   enableOffloadCmd = true;
        # };
        sync.enable = true;
        intelBusId = "PCI:0:2:0";
        nvidiaBusId = "PCI:1:0:0";
      };
      # acceptLicense = true;
    };
    # nvidiaOptimus.disable = true;
  };

  services.xserver.videoDrivers = lib.mkForce ["nvidia" "modesetting"];

  # hardware.printers = {
  #   ensurePrinters = [
  #     {
  #       name = "Xerox_WorkCentre_3025";
  #       location = "Home";
  #       deviceUri = "socket://192.168.0.38";
  #       model = "xerox/wc3025.ppd";
  #       ppdOptions = {
  #         pageSize = "A4";
  #       };
  #     }
  #   ];
  #   ensureDefaultPrinter = "Xerox_WorkCentre_3025";
  # };

  # systemd.services.nbfc_service.enable = lib.mkForce false;
}
