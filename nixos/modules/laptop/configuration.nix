{ config, pkgs, modulesPath, ... }:

{
    config = {
        environment.variables = {
            PCTYPE = "laptop";
            MUTTER_DEBUG_KMS_THREAD_TYPE="user";
        };

        # boot.extraModprobeConfig = ''
        #     blacklist nouveau
        #     options nouveau modeset=0
        # '';

        # services.udev.extraRules = ''
        #     # Remove NVIDIA USB xHCI Host Controller devices, if present
        #     ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x0c0330", ATTR{power/control}="auto", ATTR{remove}="1"
        #
        #     # Remove NVIDIA USB Type-C UCSI devices, if present
        #     ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x0c8000", ATTR{power/control}="auto", ATTR{remove}="1"
        #
        #     # Remove NVIDIA Audio devices, if present
        #     ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x040300", ATTR{power/control}="auto", ATTR{remove}="1"
        #
        #     # Remove NVIDIA VGA/3D controller devices
        #     ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x03[0-9]*", ATTR{power/control}="auto", ATTR{remove}="1"
        # '';

        boot = {
            blacklistedKernelModules = [ "nouveau" /* "nvidia" "nvidia_drm" "nvidia_modeset" */ ];
            kernelPackages = pkgs.linuxPackages_latest;
            loader.systemd-boot = {
                configurationLimit = 2;
            };
            kernelParams = [ "nvidia-drm.fbdev=1" ];
            initrd.kernelModules = [ "nvidia" "i915" "nvidia_modeset" "nvidia_uvm" "nvidia_drm" ];
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
        services.xserver.videoDrivers = [ "nvidia" ];

        services.libinput = {
            enable = true;
            touchpad = {
                naturalScrolling = true;
                tapping = false;
                clickMethod = "clickfinger";
                disableWhileTyping = true;
            };
        };

        services.ollama = {
            enable = true;
            acceleration = "cuda";
            # models = "/home/ramak/media/models";
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

        services.syncthing = {
            enable = true;
            user = "ramak";
            dataDir = "/home/ramak/dls";
            configDir = "/home/ramak/.config/syncthing";
            settings = {
                devices = {
                    "desktop" = { id = "BWFUHH5-FMJJFJO-JNDOMDV-LMIWAV2-QIJV7Y7-ZTUEPIE-V2BVDXT-QUSKLAL"; };
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
                        devices = [ "desktop" ];
                    };
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

        time.timeZone = "Asia/Hong_Kong";

        environment.systemPackages = with pkgs; [
            light
            brightnessctl
        ];
    };
}
