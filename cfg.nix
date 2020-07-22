{ config, pkgs, lib, ... }:

{
  imports = [ <musnix> <home-manager/nixos> ./nix-private ];

  environment.systemPackages = with pkgs; [
    qjackctl
    jack2
    libjack2
    supercollider
    leiningen
  ];

  nixpkgs.config.allowUnfree = true;

  home-manager.useUserPackages = true;

  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 30d";
  boot.cleanTmpDir = true;

  # Configure basic SSH access
  services.openssh.enable = true;

  hardware.enableRedistributableFirmware = true;
  networking.networkmanager.enable = true;

  users.extraUsers.jake = {
    isNormalUser = true;
    home = "/home/jake";
    hashedPassword = "";
    extraGroups = [ "wheel" "networkmanager" "audio" "jackaudio" ];
  };

  # audio play configuration
  services.jack = {
    jackd.enable = true;
    alsa.enable = false;
    loopback = { enable = true; };
  };

  # musnix.enable = true;

  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
    package = pkgs.pulseaudioFull.override { jackaudioSupport = true; };
    daemon.config = { default-sample-rate = 48000; };
  };

  sound.enable = true;
}
