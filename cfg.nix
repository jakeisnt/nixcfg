{ config, pkgs, lib, ... }:

{
  imports = [
    # <musnix>
    <home-manager/nixos>
    ./nix-private
  ];

  home-manager.useUserPackages = true;
  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 30d";
  boot.cleanTmpDir = true;
  hardware.enableRedistributableFirmware = true;

  users.users.jake = {
    isNormalUser = true;
    home = "/home/jake";
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "networkmanager" "audio" "video" "docker" ];
  };

  sound.enable = true;
  security.sudo.wheelNeedsPassword = true;
}
