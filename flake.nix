# flake.nix --- the heart of my dotfiles
#
# Author:  Henrik Lissner <henrik@lissner.net> ++ Jake Chvatal <jakechvatal@gmail.com>
# URL:     https://github.com/jakeisnt/nixcfg
# License: MIT

{
  description = "A grossly incandescent nixos config.";

  inputs = {
    # Two inputs so I can track them separately at different rates.
    nixpkgs.url = "nixpkgs/master";
    nixpkgs-unstable.url = "nixpkgs/master";

    home-manager.url = "github:rycee/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR/master";

    # Extras
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixos-hardware.url = "github:nixos/nixos-hardware";

    simple-nixos-mailserver = {
      url =
      "gitlab:simple-nixos-mailserver/nixos-mailserver";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    spicetify-nix = {
      url = "github:jakeisnt/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-remarkable = {
      url = "github:siraben/nix-remarkable";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # used to pin doom emacs version
    doom-emacs = {
      url = "github:doomemacs/doomemacs/master";
      flake = false;
    };

    # used for compatible nix build when flakes aren't yet enabled
    flake-compat = {
      url = github:edolstra/flake-compat;
      flake = false;
    };
  };

# isntweb-home, 
  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, spicetify-nix, doom-emacs, ... }:
    let
      inherit (lib) attrValues;
      inherit (lib.my) mapModules mapModulesRec mapHosts;

      system = "x86_64-linux";

      mkPkgs = pkgs: extraOverlays:
        import pkgs {
          inherit system;
          config = {
            android_sdk.accept_license = true;
            allowUnfree = true;
            permittedInsecurePackages = [ # TODO: Figure out where this is!
              "python-2.7.18.6"
            ];
          };
          overlays = extraOverlays ++ (attrValues self.overlays);
        };

      pkgs = mkPkgs nixpkgs [
        self.overlay
        (self: super: { doomEmacsRevision = doom-emacs.rev; })
      ];
      uPkgs = mkPkgs nixpkgs-unstable [];

      lib = nixpkgs.lib.extend (
        self: super: {
          my =
            import ./lib {
            inherit pkgs inputs;
            lib = self;
          };
        }
      );
    in
      {
        overlay = final: prev: {
          unstable = uPkgs;
          my = self.packages."${system}";
          extras = { inherit spicetify-nix; };
        };

        overlays = mapModules ./overlays import;

        packages."${system}" = mapModules ./packages (p: pkgs.callPackage p {});

        nixosModules = {
          dotfiles = import ./.;
        } // mapModulesRec ./modules import;

        nixosConfigurations = mapHosts ./hosts { inherit system; };

        devShell."${system}" = pkgs.mkShell {
          name = "nixos-config";
        };

        templates = {
          full = {
            path = ./.;
            description = "A grossly incandescent nixos config";
          };
          # TODO: I don't use this. Consider retiring!
          minimal = {
            path = ./templates/minimal;
            description = "A grossly incandescent and minimal nixos config";
          };
          flake = {
            path = ./templates/flake;
            description = "A simple Nix flake starter project.";
          };
        };

        defaultTemplate = self.templates.flake;

        defaultApp."${system}" = {
          type = "app";
          program = ./bin/hey;
        };
      };
}
