{ config, lib, pkgs, inputs, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    # inputs.nixos-hardware.nixosModules.dell-xps-13-9370: 'lenovo_fix.py' isn't friendly with the kernel atm
  ];

  boot = {
    consoleLogLevel = 1;
    initrd.availableKernelModules = [ "xhci_pci" "nvme" "rtsx_pci_sdmmc" ];
    initrd.kernelModules = [ "dm-snapshot" ];
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
    kernelParams = [
      # disable spectre and meltdown fixes
      "mitigations=off"
      # 4k video config
      "video=eDP-1:3840x2160@60"
      # fast quiet boot
      "quiet"
      "splash"
      "vga=current"
      "i915.fastboot=1"
      "loglevel=3"
      "systemd.show_status=auto"
      "udev.log_priority=3"
    ];
  };

  # CPU
  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";
  hardware.cpu.intel.updateMicrocode = true;

  # Power management
  environment.systemPackages = [ pkgs.acpi ];
  powerManagement.powertop.enable = true;
  # Monitor backlight control
  programs.light.enable = true;
  user.extraGroups = [ "video" ];

  # high-resolution display
  hardware.video.hidpi.enable = lib.mkDefault true;

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
    # keep file system from recording on file visit
    options = [ "noatime" ];
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-label/home";
    fsType = "ext4";
    options = [ "noatime" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/BOOT";
    fsType = "vfat";
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/786f7e92-74b5-4327-873a-89905a173f86"; }];

  # Undervolt
  #
  # Note that this is incredibly device specific.
  # If you're copying this config, there is no guarantee
  # that you'll be able to get away with these values.
  services.undervolt = {
    enable = true;
    coreOffset = -80;
    gpuOffset = -80;
    uncoreOffset = -80;
  };
}
