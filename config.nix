{ config, pkgs, lib, ... }:

{
  imports = [
    <home-manager/nixos>
    ./nix-private
  ];

  nixpkgs.config.allowUnfree = true;

  home-manager.useUserPackages = true;
  
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
    extraGroups = [ "wheel" "networkmanager" "mopidy" "audio" ];
  };

  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
  };
}
