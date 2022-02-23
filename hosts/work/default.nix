# Framework laptop!
{ config, pkgs, ... }:

{
  imports =
    [./hardware-configuration.nix ../personal.nix];

  networking.hostName = "work";

  services.getty.autologinUser = "jake";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.useDHCP = false;
  networking.interfaces.wlp170s0.useDHCP = true;

  networking.networkmanager.enable = true;
  services.xserver.libinput.enable = true;
  services.openssh.startWhenNeeded = true;

  programs.ssh = {
    startAgent = true;
    forwardX11 = true;
  };

  modules = {
    desktop.sway = {
      enable = true;
      fancy = true;
      scale = 1.5;
    };
    browsers = {
      default = "firefox";
      firefox.enable = true;
    };
    term = {
      default = "alacritty";
      alacritty.enable = true;
    };
    editors = {
      default = "nvim";
      vim.enable = true;
    };
    hardware = {
      audio.enable = true;
      bluetooth.enable = true;
    };
    media = {
      ncmpcpp.enable = true;
      recording.enable = true;
    };
    shell = {
      git.enable = true;
      lf.enable = true;
      fish.enable = true;
      gnupg = {
        enable = true;
        gui = true;
        cacheTTL = 60480000;
      };
      direnv = {
        enable = true;
        preventGC = true;
      };
    };
  };
}
