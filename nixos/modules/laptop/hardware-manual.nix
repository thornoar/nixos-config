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
    hardware.nvidia = {
        modesetting.enable = true;
        powerManagement.enable = true;
        powerManagement.finegrained = false;
        nvidiaSettings = true;
        forceFullCompositionPipeline = false;
        open = false;
        package = config.boot.kernelPackages.nvidiaPackages.production;
        prime = {
            # offload = { enable = true; enableOffloadCmd = true; };
            sync.enable = true; 
            intelBusId = "PCI:0:0:2"; 
            nvidiaBusId = "PCI:0:1:0"; 
        };
    };
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

    services.cron = {
        enable = true;
        systemCronJobs = [
            "*/1 * * * * root ${pkgs.coreutils}/bin/echo disable > /sys/firmware/acpi/interrupts/sci"
            "*/1 * * * * root ${pkgs.coreutils}/bin/echo disable > /sys/firmware/acpi/interrupts/gpe6F"
        ];
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
