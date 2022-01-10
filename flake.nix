# flake.nix --- the heart of my dotfiles
#
# Author:  Henrik Lissner <henrik@lissner.net>
# URL:     https://github.com/hlissner/dotfiles
# License: MIT
#
# Welcome to ground zero. Where the whole flake gets set up and all its modules
# are loaded.

{
  description = "A grossly incandescent nixos config.";

  inputs = {
    # Core dependencies.
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

    doom-emacs = {
      url = "github:hlissner/doom-emacs/develop";
      flake = false;
    };

    isntweb-home.url = "github:isntweb/home";

    # for getting nixos running on darwin systems
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "unstable";
    };

    # neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    # broken too frequently for me to be able to live on the bleeding edge...
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, darwin, spicetify-nix, doom-emacs, isntweb-home, ... }:
    let
      inherit (lib) attrValues;
      inherit (lib.my) mapModules mapModulesRec mapHosts;

      system = "x86_64-linux";

      mkPkgs = pkgs: extraOverlays:
        import pkgs {
          inherit system;
          config = {
            android_sdk.accept_license = true;
            allowUnfree = true; # forgive me Stallman senpai
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

	# x86 macos: everything is still compatible
	# TODO make a darwin configuration
	# heavily inspired by https://github.com/shaunsingh/nix-darwin-dotfiles/tree/c56a7b2ac40c4a61a5c385b4752d23cbb26e5e81
	# copy that repo for further progress
	darwinConfigurations."trc-laptop" = darwin.lib.darwinSystem {
		inherit system; # change this if on ARM
		modules = {
		({ pkgs, lib, ... }: {
          services.nix-daemon.enable = true;
          security.pam.enableSudoTouchIdAuth = true;
          nixpkgs = {
            config.allowUnfree = true;
            overlays = with inputs; [
              nur.overlay
              spacebar.overlay
              neovim-overlay.overlay
              rust-overlay.overlay
              (final: prev: {
                doomEmacsRevision = inputs.doom-emacs.rev;
                sf-mono-liga-bin = pkgs.callPackage ./pkgs/sf-mono-liga-bin { };
                neovide = pkgs.callPackage ./pkgs/neovide { };
                yabai = let
                  version = "4.0.0-dev";
                  buildSymlinks = prev.runCommand "build-symlinks" { } ''
                    mkdir -p $out/bin
                    ln -s /usr/bin/xcrun /usr/bin/xcodebuild /usr/bin/tiffutil /usr/bin/qlmanage $out/bin
                  '';
                in prev.yabai.overrideAttrs (old: {
                  inherit version;
                  src = inputs.yabai-src;

                  buildInputs = with prev.darwin.apple_sdk.frameworks; [
                    Carbon
                    Cocoa
                    ScriptingBridge
                    prev.xxd
                    SkyLight
                  ];

                  nativeBuildInputs = [ buildSymlinks ];
                });
                alacritty-ligatures = with pkgs;
                  (alacritty.overrideAttrs (old: rec {
                    src = inputs.alacritty-src;
                    cargoDeps = old.cargoDeps.overrideAttrs (_: {
                      inherit src;
                      outputHash =
                        "sha256-tY5sle1YUlUidJcq7RgTzkPsGLnWyG/3rtPqy2GklkY=";
                    });
                  }));
                emacs = (prev.emacs.override {
                  srcRepo = true;
                  nativeComp = true;
                  withSQLite3 = true;
                  withXwidgets = true;
                }).overrideAttrs (o: rec {
                  version = "29.0.50";
                  src = inputs.emacs-src;

                  buildInputs = o.buildInputs ++ [
                    prev.darwin.apple_sdk.frameworks.WebKit
                    pkgs.cairo
                    pkgs.harfbuzz
                  ];

                  configureFlags = o.configureFlags ++ [
                    "--with-rsvg"
                    "--with-threads"
                    "--without-gpm"
                    "--without-dbus"
                    "--without-mailutils"
                    "--without-toolkit-scroll-bars"
                    "--without-pop"
                  ];

                  patches = [
         #           ./patches/fix-window-role.patch
                    # ./patches/fix-harfbuzz-check.patch
                    # ./patches/no-titlebar.patch
                  ];

                  postPatch = o.postPatch + ''
                    substituteInPlace lisp/loadup.el \
                    --replace '(emacs-repository-get-branch)' '"master"'
                  '';

                  CFLAGS =
                    "-DMAC_OS_X_VERSION_MAX_ALLOWED=110203 -g -O3 -mtune=native -march=native -fomit-frame-pointer";
                });
              })
            ];
          };
          nix = {
            package = pkgs.nix;
            extraOptions = ''
              system = aarch64-darwin
              extra-platforms = aarch64-darwin x86_64-darwin
              experimental-features = nix-command flakes
              build-users-group = nixbld
            '';
          };
          environment.systemPackages = with pkgs; [
            # emacs needs to be here since its a GUI app
            emacs

            # Build Tools
            jdk
            rust-bin.nightly.latest.default

            # Language Servers
            nodePackages.pyright
            rust-analyzer

            # Formatting
            nixfmt
            black
            shellcheck

            # Terminal utils and rust alternatives :tm:
            xcp
            lsd
            procs
            tree
            zoxide
            bottom
          ];
          fonts = {
            enableFontDir = true;
            fonts = with pkgs; [
              overpass
              fira
              emacs-all-the-icons-fonts
              sf-mono-liga-bin
            ];
          };
        })	
		};
		
	};

        devShell."${system}" = pkgs.mkShell {};

        templates = {
          full = {
            path = ./.;
            description = "A grossly incandescent nixos config";
          };
          minimal = {
            path = ./templates/minimal;
            description = "A grossly incandescent and minimal nixos config";
          };
        };

        defaultTemplate = self.templates.minimal;

        defaultApp."${system}" = {
          type = "app";
          program = ./bin/hey;
        };
      };
}
