{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ../personal.nix ];

  networking.hostName = "vultr";

  modules = {
    editors = {
      default = "nvim";
      vim.enable = true;
    };
    shell = {
      git.enable = true;
      ranger.enable = true;
      zsh.enable = true;
      gnupg.enable = true;
    };
    services = {
      mailserver.enable = true;
      ssh.enable = true;
      matrix = {
        enable = true;
        registration = true;  
      };
      nginx.enable = true;
    };
    theme.active = "nordic";
  };

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/vda";
  
  # The ports are configured in their respective services,
  # but the system should individually decide whether to enable the firewall
  networking.firewall.enable = true;

  # Enable the OpenSSH daemon.
  networking.useDHCP = false;
  networking.interfaces.ens3.useDHCP = true;
}
