{ config, lib, pkgs, ... }:

{
  imports = [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix> ];

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelParams = [ "video=eDP-1:3940x1920@60" ];
  # boot.kernelParams = [
  #   "ipv6.disable=0"
  #   "video=eDP-1:1440x810@60"
  #   "pcie_aspm=force"
  #   "resume=/dev/nvme0n1p3"
  #   "iwlwifi.power_save=Y"
  #   "acpi_brightness=vendor"
  #   "i915.enable_rc6=7"
  #   "i915.enable_psr=2"
  #   "i915.enable_fbc=1"
  #   "i915.lvds_downclock=1"
  #   "i915.semaphores=1"
  # ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "vfat";
  };

  swapDevices = [{ device = "/dev/disk/by-label/swap"; }];

  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  # High-DPI console
  console.font =
    lib.mkDefault "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
}
