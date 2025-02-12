{ config, pkgs, modulesPath, ... }:
{
    boot = {
        blacklistedKernelModules = [ "nouveau" /* "nvidia" "nvidia_drm" "nvidia_modeset" */ ];
        kernelPackages = pkgs.linuxPackages_latest;
        loader.systemd-boot = {
            configurationLimit = 2;
        };
    };

    nixpkgs.config.nvidia.acceptLicense = true;
    hardware.graphics = {
        enable = true;
        enable32Bit = true;
    };
    powerManagement = {
        enable = true;
        powertop.enable = true;
        cpuFreqGovernor = "powersave";
        resumeCommands = "${pkgs.kmod}/bin/rmmod atkbd; ${pkgs.kmod}/bin/modprobe atkbd reset=1";
    };

    fileSystems."/home/ramak/media" = {
        device = "/dev/disk/by-uuid/aa543ce3-5cbd-4251-a01c-59ebe4a97f92";
        fsType = "ext4";
        options = [ "nofail" "rw" "user" "auto" ];
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
        # tlp = {
        #     enable = true;
        #     settings = {
        #         TLP_DEFAULT_MODE = "AC";
        #
        #         CPU_SCALING_GOVERNOR_ON_AC = "performance";
        #         CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
        #
        #         CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
        #         CPU_ENERGY_PERF_POLICY_ON_AC = "performance";
        #
        #         CPU_MIN_PERF_ON_AC = 0;
        #         CPU_MAX_PERF_ON_AC = 100;
        #         CPU_MIN_PERF_ON_BAT = 0;
        #         CPU_MAX_PERF_ON_BAT = 40;
        #
        #         CPU_BOOST_ON_AC = 1;
        #         CPU_BOOST_ON_BAT = 0;
        #
        #         # START_CHARGE_THRESH_BAT0 = 40; # 40 and bellow it starts to charge
        #         # STOP_CHARGE_THRESH_BAT0 = 80; # 80 and above it stops charging
        #     };
        # };
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
        settings = {
            General.Experimental = true;
        };
    };
}
