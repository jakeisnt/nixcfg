{ config, pkgs, lib, ... }:

{
  imports = [ ../../arch/pi ];

  environment.systemPackages = with pkgs; [ raspberrypi-tools ];
  networking.hostName = "pi";
  # Preserve space by sacrificing documentation and history
  documentation.nixos.enable = false;
  home-manager.users.jake = import ../../home/headless.nix;
}

