  # Framework laptop!
{ config, pkgs, inputs, lib, ... }:

{
  imports = [./hardware-configuration.nix ../personal.nix];

  networking.hostName = "work";
  services.getty.autologinUser = "jake";

  # Keep the Framework available as a server, including with the lid closed.
  services.logind.settings.Login = {
    HandleLidSwitch = "ignore";
    HandleLidSwitchExternalPower = "ignore";
    HandleLidSwitchDocked = "ignore";
    HandleSuspendKey = "ignore";
    HandleHibernateKey = "ignore";
    IdleAction = "ignore";
  };
  systemd.sleep.settings.Sleep = {
    AllowSuspend = false;
    AllowHibernation = false;
    AllowHybridSleep = false;
    AllowSuspendThenHibernate = false;
  };

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  time.timeZone = "Europe/Stockholm";

  user.packages = with pkgs; [
    ripgrep
    fzf
    nil
  ];

  # automount storage devices
  services.devmon.enable = true;
  user.extraGroups = [
    # unprivileged access to storage devices
    "storage"
    # for scanner
    "scanner"
  ];

  networking = {
    useDHCP = false;
    interfaces.wlp170s0.useDHCP = true;
    networkmanager = {
      enable = true;
      wifi = {
        powersave = false; # no wifi lag
      };
    };
  };

  services.libinput.enable = true;
  services.tailscale = {
    enable = true;
    openFirewall = true;
    # Authenticate tailnet SSH connections using Tailscale identities.
    extraSetFlags = [ "--ssh" ];
  };
  modules.services.ssh.enable = true;
  services.openssh = {
    startWhenNeeded = false;
    openFirewall = false;
  };
  # Mosh starts through SSH, then uses UDP for the terminal session.
  programs.mosh = {
    enable = true;
    openFirewall = false;
  };
  networking.firewall.interfaces.${config.services.tailscale.interfaceName} = {
    allowedTCPPorts = [ 22 ];
    allowedUDPPortRanges = [
      { from = 60000; to = 61000; }
    ];
  };
  programs.ssh = {
    startAgent = true;
    forwardX11 = true;
  };

  # Share the running desktop through an SSH tunnel, once per Sway session.
  home.configFile."sway/config".text = lib.mkAfter ''

    exec ${pkgs.wayvnc}/bin/wayvnc 127.0.0.1 5900
  '';

  modules = {
    desktop.sway = {
      enable = true;
      fancy = true;
      scale = 1.0;
    };
    browsers = {
      default = "chrome";
      chrome.enable = true;
    };
    editors = {
      default = "nvim";
      vim.enable = true;
      emacs.enable = false;
    };
    hardware = {
      audio.enable = true;
      # bluetooth.enable = true;
    };
    # messengers.email.enable = true;
    media = {
      # ncmpcpp.enable = true; # disabled: mopidy/mpd unreliable, python310 dep
      recording.enable = true;
      # TODO:  These options require python2.
      # graphics.enable = true;
      # graphics.photo.enable = true;
    };
    dev.ai.enable = true;
    shell = {
      git.enable = true;
      file.enable = true;
      nushell.enable = true;
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
