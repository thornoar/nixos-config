{ config, pkgs, modulesPath, ... }:

{
    config = {
        environment.variables = {
            PCTYPE = "laptop";
            MUTTER_DEBUG_KMS_THREAD_TYPE="user";
        };

        boot.extraModprobeConfig = ''
            blacklist nouveau
            options nouveau modeset=0
        '';

        services.udev.extraRules = ''
            # Remove NVIDIA USB xHCI Host Controller devices, if present
            ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x0c0330", ATTR{power/control}="auto", ATTR{remove}="1"

            # Remove NVIDIA USB Type-C UCSI devices, if present
            ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x0c8000", ATTR{power/control}="auto", ATTR{remove}="1"

            # Remove NVIDIA Audio devices, if present
            ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x040300", ATTR{power/control}="auto", ATTR{remove}="1"

            # Remove NVIDIA VGA/3D controller devices
            ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x03[0-9]*", ATTR{power/control}="auto", ATTR{remove}="1"
        '';

        boot.blacklistedKernelModules = [ "nouveau" "nvidia" "nvidia_drm" "nvidia_modeset" ];

        boot.kernelPackages = pkgs.linuxPackages_latest;

        boot.loader.systemd-boot = {
            configurationLimit = 2;
        };

        nixpkgs.config.nvidia.acceptLicense = true;
        hardware.nvidia = {
            modesetting.enable = true;
            powerManagement.enable = false;
            powerManagement.finegrained = false;
            nvidiaSettings = false;
            forceFullCompositionPipeline = false;
            open = false;
            package = config.boot.kernelPackages.nvidiaPackages.beta;
            prime = {
                offload = {
                    enable = true;
                    enableOffloadCmd = true;
                };
                # sync.enable = true; 
                intelBusId = "PCI:0:2:0"; 
                nvidiaBusId = "PCI:1:0:0"; 
            };
        };
        hardware.graphics = {
            enable = true;
            enable32Bit = true;
        };

        services.libinput = {
            enable = true;
            touchpad = {
                naturalScrolling = true;
                tapping = false;
                clickMethod = "clickfinger";
                disableWhileTyping = true;
            };
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
            #         # CPU_BOOST_ON_AC = 1;
            #         # CPU_BOOST_ON_BAT = 0;
            #         # CPU_SCALING_GOVERNOR_ON_AC = "performance";
            #         # CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
            #         START_CHARGE_THRESH_BAT0 = 50;
            #         STOP_CHARGE_THRESH_BAT0 = 80;
            #     };
            # };
        };
        powerManagement = {
            enable = true;
            powertop.enable = true;
            cpuFreqGovernor = "powersave";
            resumeCommands = "${pkgs.kmod}/bin/rmmod atkbd; ${pkgs.kmod}/bin/modprobe atkbd reset=1";
        };

        services.cron = {
            enable = true;
            systemCronJobs = [
                "*/1 * * * * root ${pkgs.coreutils}/bin/echo disable > /sys/firmware/acpi/interrupts/sci"
                "*/1 * * * * root ${pkgs.coreutils}/bin/echo disable > /sys/firmware/acpi/interrupts/gpe6F"
                # "@reboot root ${pkgs.coreutils}/bin/echo disable > /sys/firmware/acpi/interrupts/sci"
                # "@reboot root ${pkgs.coreutils}/bin/echo disable > /sys/firmware/acpi/interrupts/gpe6F"
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
        # systemd.timers."interrupt-disable" = {
        #     wantedBy = [ "timers.target" ];
        #     timerConfig = {
        #         OnBootSec = "3m";
        #         OnUnitActiveSec = "1";
        #         Unit = "interrupt-disable.service";
        #     };
        # };
        # systemd.services."interrupt-disable" = {
        #     script = ''
        #         echo disable > /sys/firmware/acpi/interrupts/gpe6F
        #     '';
        #     serviceConfig = {
        #         Type = "oneshot";
        #         User = "ramak";
        #     };
        #     wantedBy = [ "multi-user.target" ];
        # };

        services.syncthing.settings = {
            devices = {
                "station" = { id = "BWFUHH5-FMJJFJO-JNDOMDV-LMIWAV2-QIJV7Y7-ZTUEPIE-V2BVDXT-QUSKLAL"; };
            };
            folders = {
                "music" = {
                    path = "~/media/music";
                    ignorePerms = false;
                    devices = [];
                };
                "books" = {
                    path = "~/media/books";
                    ignorePerms = false;
                    devices = [];
                };
                "sandbox" = {
                    path = "~/projects/sandbox";
                    ignorePerms = false;
                    devices = [ "station" ];
                };
            };
        };

        hardware.bluetooth = {
            enable = true;
            powerOnBoot = true;
            settings = {
                General.Experimental = true;
            };
        };

        time.timeZone = "Europe/Belgrade";

        environment.systemPackages = with pkgs; [
            light
            brightnessctl
        ];
    };
}
