# Minimal config built to run off of a live USB.
# Provides a TUI with configured emacs, git and zsh.
# Used for bootstrapping other configurations!

{ config, lib, pkgs, ... }:
let hostname = "usb";
in

{
  imports = [
    ../personal.nix
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-base.nix>
  ];

  isoImage = {
    # if i don't want this to be an install disk:
    # makeEfiBootable = true;
    # makeUsbBootable = true;
    volumeID = lib.mkForce hostname;
    isoName = lib.mkForce "${hostname}.iso";
  };

  networking = {
    networkmanager.enable = true;
    wireless.enable = lib.mkForce false;
  };

  security.polkit.extraConfig = ''
    polkit.addRule(function(action, subject) {
      if (subject.isInGroup("wheel")) {
        return polkit.Result.YES;
      }
    });
  '';

  modules = {
    editors = {
      default = "emacs";
      emacs.enable = true;
    };
    shell = {
      git.enable = true;
      lf.enable = true;
      zsh.enable = true;
    };
  };
}
