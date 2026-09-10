  # Framework laptop!
{ config, pkgs, inputs, lib, ... }:

{
  imports = [./hardware-configuration.nix ../personal.nix];

  networking.hostName = "work";
  # Keep the machine at a normal login prompt.  Sway remains available from
  # the tty when this laptop is needed interactively.
  modules.shell.loginInit = lib.mkForce "";
  home.configFile."nushell/login.nu" = lib.mkForce { text = ""; };

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
    # NetworkManager owns DHCP and Wi-Fi on this host.
    dhcpcd.enable = false;
    networkmanager = {
      enable = true;
      wifi = {
        powersave = false; # no wifi lag
      };
    };
  };

  # Bluetooth is not needed on the server installation.  The generated
  # hardware file enables it, so override that setting for this host.
  hardware.bluetooth.enable = lib.mkForce false;

  services.libinput.enable = true;
  services.tailscale = {
    enable = true;
    package = pkgs.unstable.tailscale;
    openFirewall = true;
    # Authenticate tailnet SSH connections using Tailscale identities.
    extraSetFlags = [ "--ssh" ];
  };
  # The clock can reset on boot, making Tailscale's TLS login fail.
  # Wait for NTP before reconnecting with the saved Tailscale identity.
  services.timesyncd.enable = true;
  systemd.additionalUpstreamSystemUnits = [ "systemd-time-wait-sync.service" ];
  systemd.services.systemd-time-wait-sync.wantedBy = [ "time-sync.target" ];
  systemd.services.tailscaled = {
    wants = [ "network-online.target" "time-sync.target" ];
    after = [ "network-online.target" "time-sync.target" ];
  };
  # Bring the saved login online even if Tailscale was previously stopped.
  # Run as root at boot and whenever the daemon restarts, without a desktop login.
  systemd.services.tailscaled-autoconnect = {
    description = "Connect Tailscale using the saved login";
    wantedBy = [ "multi-user.target" "tailscaled.service" ];
    wants = [ "tailscaled.service" ];
    after = [ "tailscaled.service" ];
    partOf = [ "tailscaled.service" ];
    startLimitIntervalSec = 0;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      # No preference flags: preserve the existing login and network settings.
      ExecStart = "${lib.getExe config.services.tailscale.package} up";
      TimeoutStartSec = "30s";
      Restart = "on-failure";
      RestartSec = "10s";
    };
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
    allowedTCPPorts = [ 22 5900 ];
    allowedUDPPortRanges = [
      { from = 60000; to = 61000; }
    ];
  };
  programs.ssh = {
    startAgent = true;
    forwardX11 = true;
  };

  # Share the running desktop on Tailscale, once per Sway session.
  home.configFile."sway/config".text = lib.mkAfter ''

    exec ${pkgs.writeShellScript "wayvnc-tailscale" ''
      # Sway may start before Tailscale has an address.
      until address=$(${config.services.tailscale.package}/bin/tailscale ip -4 2>/dev/null) && [ -n "$address" ]; do
        ${pkgs.coreutils}/bin/sleep 2
      done
      exec ${pkgs.wayvnc}/bin/wayvnc "$address" 5900
    ''}
  '';

  modules = {
    desktop.sway = {
      enable = true;
      fancy = true;
      scale = 1.0;
    };
    # Keep the Framework reachable over SSH/Tailscale while idle.
    wayland.swaylock.idle.enable = false;
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
    dev = {
      ai.enable = true;
      docker = {
        enable = true;
        podman = true;
      };
    };
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
