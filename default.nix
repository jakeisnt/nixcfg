{ inputs, config, lib, pkgs, ... }:

with lib;
with lib.my;
with inputs; {
  imports =
    [
      home-manager.nixosModules.home-manager
      simple-nixos-mailserver.nixosModules.mailserver
    ]
    ++ (mapModulesRec' (toString ./modules) import);

  # Common config for all nixos machines to ensure the flake operates soundly
  environment.variables = {
    DOTFILES = dotFilesDir;
    NIXPKGS_ALLOW_UNFREE = "1";
  };

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';

    nixPath = (mapAttrsToList (n: v: "${n}=${v}") inputs) ++ [
      "nixpkgs-overlays=${dotFilesDir}/overlays"
      "dotfiles=${dotFilesDir}"
    ];

    settings.substituters = [
      "https://cache.nixos.org/"
      "https://nix-community.cachix.org"
    ];

    settings.trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];

    # TODO: I had conflicting values here,
    # so had to update. Makes sense -- but what do each of these do?
    registry = {
      nixos.flake = nixpkgs;
      nixpkgs.flake = nixpkgs; # nixpkgs-unstable;
    };

    settings.sandbox = true;
    settings.trusted-users = [ username ];
    gc.automatic = false; # never automatically garbage collect
  };
  system.configurationRevision = mkIf (self ? rev) self.rev;
  system.stateVersion = "20.09";

  # allow users to store fonts easily
  fonts.fontDir.enable = true;

  ## Some reasonable, global defaults
  # This is here to appease 'nix flake check' for generic hosts with no
  # hardware-configuration.nix or fileSystem config.
  fileSystems."/".device = mkDefault "/dev/disk/by-label/nixos";

  # Use the latest kernel
  # boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot.configurationLimit = 10;
    systemd-boot.enable = mkDefault true;
  };

  environment.systemPackages = with pkgs; [
    coreutils
    wget
    unzip
  ];
}
