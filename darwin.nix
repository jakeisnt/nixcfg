# darwin.nix --- root module for macOS (nix-darwin) hosts #
# Analogous to default.nix for NixOS, but scoped to what nix-darwin supports.

{ inputs, config, lib, pkgs, ... }:

with lib;
with lib.my;
with inputs; {
  imports =
    [
      inputs."home-manager-unstable".darwinModules.home-manager
      inputs.determinate.darwinModules.default
    ]
    ++ [
      # Darwin-specific option wiring (user.packages, home.*, env aliases)
      ./modules/_darwin-options.nix

      # Shell modules verified safe on darwin
      ./modules/shell/nushell.nix   # defines modules.shell.loginInit (needed by fish)
      ./modules/shell/direnv.nix
      ./modules/shell/gnupg.nix     # defines modules.shell.gnupg.enable (needed by git)
      ./modules/shell/git.nix
      ./modules/shell/fish.nix

      # Dev modules
      ./modules/dev/node.nix
      ./modules/dev/rust.nix
    ];

  environment.variables = {
    DOTFILES = dotFilesDir;
    NIXPKGS_ALLOW_UNFREE = "1";
  };

  # Determinate Nix owns the daemon and /etc/nix/nix.conf; the determinate
  # module disables nix-darwin's Nix management and writes these settings to
  # /etc/nix/nix.custom.conf, which Determinate's nix.conf includes.
  determinateNix.customSettings = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://ryoppippi.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "ryoppippi.cachix.org-1:b2LbtWNvJeL/qb1B6TYOMK+apaCps4SCbzlPRfSQIms="
    ];
  };

  environment.systemPackages = with pkgs; [
    coreutils
    wget
    unzip
  ];

  # Required by nix-darwin; bump this when upgrading nix-darwin major versions.
  system.stateVersion = 6;
}
