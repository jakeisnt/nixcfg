{ config, pkgs, lib, ... }:

{
  imports = [
    (import "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos")
  ];
  # NixOS wants to enable GRUB by default
  boot.loader.grub.enable = false;

  # fix for boot bug in 5.x kernel
  boot.kernelPackages = pkgs.linuxPackages_4_19;

  # A bunch of boot parameters needed for optimal runtime on RPi 3b+
  boot.kernelParams = ["cma=256M"];
  boot.loader.raspberryPi.enable = true;
  boot.loader.raspberryPi.version = 3;
  boot.loader.raspberryPi.uboot.enable = true;
  boot.loader.raspberryPi.firmwareConfig = ''
    gpu_mem=256
  '';

  environment.systemPackages = with pkgs; [
    raspberrypi-tools
    git
    tmux
    neovim
    networkmanager
  ];

  # File systems configuration for using the installer's partition layout
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
    };
  };

  # Preserve space by sacrificing documentation and history
  documentation.nixos.enable = false;
  
  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 30d";
  boot.cleanTmpDir = true;

  # Configure basic SSH access
  services.openssh.enable = true;

  # Use 1GB of additional swap memory in order to not run out of memory
  # when installing lots of things while running other things at the same time.
  swapDevices = [ { device = "/swapfile"; size = 1024; } ];
 
  hardware.enableRedistributableFirmware = true;
  networking.networkmanager.enable = true;
  
  users.extraUsers.jake = {
    isNormalUser = true;
    home = "/home/jake";
    hashedPassword = "";
    extraGroups = [ "wheel" "networkmanager" ];
  };

  programs.home-manager.enable = true;

  programs.neovim = {
    package = pkgs.neovim;
    enable = true;

    viAlias = true;
    vimAlias = true;
    withNodeJs = true;
  };

  # nvim.sessionVariables.EDITOR = "nvim";
}
