{ lib, pkgs, ...}: {

  # raspi system config
  config = lib.mkDefault {
    boot = {
      # NixOS wants to enable GRUB by default
      loader.grub.enable = false;
      loader.generic-extlinux-compatible.enable = true;

      # fix for boot bug in 5.x kernel
      kernelPackages = pkgs.linuxPackages_4_19;

      # A bunch of boot parameters needed for optimal runtime on RPi 3b+
      kernelParams = ["cma=256M"];
      loader.raspberryPi.enable = true;
      loader.raspberryPi.version = 3;
      loader.raspberryPi.uboot.enable = true;
      loader.raspberryPi.firmwareConfig = ''
        gpu_mem=256
      '';
    };

    # File systems configuration for using the installer's partition layout
    fileSystems = {
      "/" = {
        device = "/dev/disk/by-label/NIXOS_SD";
        fsType = "ext4";
      };
    };

    # Use 1GB of additional swap memory in order to not run out of memory
    # when installing lots of things while running other things at the same time.
    swapDevices = [ { device = "/swapfile"; size = 1024; } ];
  };
}
