{ inputs, config, lib, pkgs, ... }:

with lib;
with lib.my;
with inputs; {
  imports =
    # I use home-manager to deploy files to $HOME; little else
    [
      home-manager.nixosModules.home-manager
      simple-nixos-mailserver.nixosModules.mailserver
    ]
    # All my personal modules
    ++ (mapModulesRec' (toString ./modules) import);

  # Common config for all nixos machines to ensure the flake operates soundly
  environment.variables.DOTFILES = dotFilesDir;

  # Configure nix and nixpkgs
  environment.variables.NIXPKGS_ALLOW_UNFREE = "1";
  nix = {
    package = pkgs.nixFlakes;
    # Use Flakes
    # Use a remote builder when it has faster connection
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    nixPath = (mapAttrsToList (n: v: "${n}=${v}") inputs) ++ [
      "nixpkgs-overlays=${dotFilesDir}/overlays"
      "dotfiles=${dotFilesDir}"
    ];
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://hydra.iohk.io"
      "https://nix-community.cachix.org"
      "https://nix-remarkable.cachix.org"
    ];
    binaryCachePublicKeys =
      [
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "nix-remarkable.cachix.org-1:1VrbWNGmWFgi0Wu6iJl7m/RkO8SUqLdryfzLbQQ5Tqc="
      ];
    registry = {
      nixos.flake = nixpkgs;
      nixpkgs.flake = nixpkgs-unstable;
    };
    useSandbox = true;
    trustedUsers = [ username ];
    gc.automatic = false; # never automatically garbage collect
  };
  system.configurationRevision = mkIf (self ? rev) self.rev;
  system.stateVersion = "20.09";

  ## Some reasonable, global defaults
  # This is here to appease 'nix flake check' for generic hosts with no
  # hardware-configuration.nix or fileSystem config.
  fileSystems."/".device = mkDefault "/dev/disk/by-label/nixos";


  sops.defaultSopsFile = ./secrets.yaml;
  sops.sshKeyPaths = [ "/home/jake/.ssh/id_rsa" ];

  # Use the latest kernel
  # boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot.configurationLimit = 10;
    systemd-boot.enable = mkDefault true;
  };

  # let's get started!
  environment.systemPackages = with pkgs; [
    # nix tools
    nixpkgs-fmt
    # linux good to haves
    coreutils
    wget
    file # check type of files
    h # fun program
    gnumake
    # offer some convenience as needed
    unzip
    cmake

    # nice nttworking utils
    httpie
    curlie
  ];
}
