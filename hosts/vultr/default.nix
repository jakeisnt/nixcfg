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
    };
    theme.active = "nordic";
  };

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/vda";

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      22   # SSH
      8448 # Matrix
      80   # Standard

      # Email
      25   # SMTP
      465  # Submission TLS
      587  # Submission StartTLS
      993  # IMAP with TLS
      995  # POP3 with TLS
      143  # IMAP with StartTLS
      110  # POP3 with StartTLS
    ];
  };

  # Enable the OpenSSH daemon.
  networking.useDHCP = false;
  networking.interfaces.ens3.useDHCP = true;
}
