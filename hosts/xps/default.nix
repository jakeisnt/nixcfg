{ config, lib, pkgs, ... }:

{
  imports = [ <nixos-hardware/dell/xps/13-9370> ./hardware-configuration.nix ];

  home-manager.users.jake = import ../../home/gui.nix;

  services.xserver = {
    enable = true;
    autorun = true;
    layout = "us";
    xkbOptions = "caps:swapescape";

    desktopManager = { xterm.enable = false; };

    displayManager.startx.enable = false;

    windowManager.session = lib.singleton {
      name = "exwm";
      start = ''
        emacs --daemon -f exwm-enable
        emacsclient -c
      '';
    };

    # 406 228 work at 250%
    # 508 285 for 200%
    # 1016 571 for 100%
    monitorSection = ''
      DisplaySize 508 285
    '';
  };

  # console uses x-server keyboard settings
  # console.useXkbConfig = true;

  boot.plymouth.enable = true; # trying out alt boot screen

  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    systemService = true;
    user = "jake";
    group = "wheel";
    dataDir = "/home/jake";
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # GRUB boot loader configuration can be found here
  # boot.loader.grub.enable = true;
  # boot.loader.grub.version = 2;
  # boot.loader.grub.device = "/dev/nvme0n1p1";
  # boot.loader.grub.useOSProber = true;
  # boot.loader.grub.efiSupport = true;

  networking.hostName = "xps"; # Define your hostname.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp2s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    # font = "latarcyrheb-sun32";
    keyMap = "us";
  };

  # Set your time zone.
  # time.timeZone = "America/Amsterdam";
  services.localtime.enable = true;

  environment.systemPackages = with pkgs; [
    wget
    git
    pulseaudio-ctl
    (mumble.override { pulseSupport = true; })
  ];

  programs.light.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  #   pinentryFlavor = "gnome3";
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  networking.networkmanager.enable = true;

  # enable touchpad
  services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.jake = {
    isNormalUser = true;
    extraGroups =
      [ "wheel" "networkmanager" "video" ]; # Enable ‘sudo’ for the user.
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?
}
