{ config, pkgs, lib, ... }:

{
  imports = [
    <home-manager/nixos>
    ./arch/pi
  ];



  # Preserve space by sacrificing documentation and history
  documentation.nixos.enable = false;

  nixpkgs.config.allowUnfree = true;

  home-manager.useUserPackages = true;
  home-manager.users.jake = import ./home.nix;

  environment.systemPackages = with pkgs; [
    ranger
    networkmanager
    raspberrypi-tools
  ];

  
  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 30d";
  boot.cleanTmpDir = true;

  # Configure basic SSH access
  services.openssh.enable = true;
 
  hardware.enableRedistributableFirmware = true;
  networking.networkmanager.enable = true;
  
  users.extraUsers.jake = {
    isNormalUser = true;
    home = "/home/jake";
    hashedPassword = "";
    extraGroups = [ "wheel" "networkmanager" ];
  };

  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
  };
}
