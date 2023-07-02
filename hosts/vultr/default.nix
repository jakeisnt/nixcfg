{ config, pkgs, lib, ... }:
with lib.my; {
  imports = [ ./hardware-configuration.nix ../personal.nix ];

  networking.hostName = "vultr";

  modules = {
    # just use nano
    # editors = {
    #   default = "nvim";
    #   vim.enable = true;
    # };
    shell = {
      git.enable = true;
      file.enable = true;
      fish.enable = true; # TODO need a simple shell to use without rust things, but not zsh - too heavy
      gnupg.enable = true;
      direnv.enable = true;
    };
    services = {
      acme.enable = true;
      mailserver.enable = false;
      ssh.enable = true;
      matrix = {
        enable = false;
        registration = false;
        element = false;
      };
      bitwarden = {
        enable = false;
        registration = false;
        mail = false;
      };
      cal.enable = false;
      nginx.enable = true;
    };
  };

  boot.loader.systemd-boot.enable = false;

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/vda";
  };

  networking = {
    domain = "isnt.online";
    # The ports are configured in their respective services,
    # but the system should individually decide whether to enable the firewall
    firewall.enable = true;

    # Enable the OpenSSH daemon.
    useDHCP = false;
    interfaces.ens3.useDHCP = true;
  };

  # isntweb-home.enable = true;
  services.nginx.virtualHosts = {
    "isnt.online" = {
      forceSSL = true;
      enableACME = true;
      locations = {
        "/" = {
          proxyPass = "http://127.0.0.1:6200/";
          proxyWebsockets = true;
        };
      };
    };
  };
}
