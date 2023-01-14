{ config, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
# emacs (~pgtk~) + native-comp
# this was `29.2`, but the emacs interface keeps changing in backwards-incompatible ways
let
  myemacs0 = pkgs.emacsUnstable;
  emacsWPkgs = (pkgs.emacsPackagesFor myemacs0).emacsWithPackages;
  myemacs = emacsWPkgs (epkgs: (with epkgs; [ vterm pdf-tools org-pdftools ]));

  cfg = config.modules.editors.emacs;
  # Install Doom Emacs if not already configured, placing the correct things in the system path.
  daemonScript = pkgs.writeScriptBin "emacs" ''
    #!${pkgs.bash}/bin/bash -l
    export PATH=$PATH:${lib.makeBinPath [ pkgs.git pkgs.sqlite pkgs.unzip ]}
    export SHELL=${pkgs.bash}/bin/bash
    if [ ! -d $HOME/.emacs.d/.git ]; then
      mkdir -p $HOME/.emacs.d
      git -C $HOME/.emacs.d init
    fi
    if [ ! -d $HOME/.config/doom ]; then
      git clone git@github.com:jakeisnt/doom.d.git $HOME/.config/doom
    fi
    if [ $(git -C $HOME/.emacs.d rev-parse HEAD) != ${pkgs.doomEmacsRevision} ]; then
      git -C $HOME/.emacs.d fetch https://github.com/hlissner/doom-emacs.git || true
      git -C $HOME/.emacs.d checkout ${pkgs.doomEmacsRevision} || true
      $HOME/.emacs.d/bin/doom sync || true
      YES=1 FORCE=1 $HOME/.emacs.d/bin/doom sync -u &
    fi

    exec ${myemacs}/bin/emacs --daemon
  '';

in {
  options.modules.editors.emacs = {
    enable = mkBoolOpt false;
    daemon = mkBoolOpt false;
    doom = {
      enable = mkBoolOpt true;
      fromSSH = mkBoolOpt false;
    };
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [
      inputs.emacs-overlay.overlay
      inputs.nur.overlay
    ];

    # enable tree sitter grammars
    modules.editors.tree-sitter.enable = true;

    services.emacs = mkIf cfg.daemon {
      enable = true;
      install = false;
      package = daemonScript;
      defaultEditor = false; # configured elsewhere
    };

    user.packages = with pkgs; [
      binutils # native-comp needs 'as', provided by this
      myemacs
      daemonScript

      ## Doom dependencies
      git
      (ripgrep.override { withPCRE2 = true; })
      gnutls # for TLS connectivity

      ## Optional dependencies
      fd # faster projectile indexing
      imagemagick # for image-dired
      (mkIf (config.programs.gnupg.agent.enable)
        pinentry_emacs) # in-emacs gnupg prompts
      zstd # for undo-fu-session/undo-tree compression

      ## Module dependencies
      # :checkers spell
      (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
      # :checkers grammar
      languagetool
      # :tools editorconfig
      editorconfig-core-c # per-project style config
      # :tools lookup & :lang org +roam
      sqlite
      # :lang latex & :lang org (latex previews)
      texlive.combined.scheme-medium
      # :lang rust
      # org +gnuplot
      gnuplot
      # org +pandoc
      pandoc

      # for vterm
      libvterm
      libtool

      # for org-roam graph
      graphviz
      pinentry-emacs

      # presumably to build emacs
      gcc

      # copilot
      nodejs
    ];

    env.PATH = [ "$XDG_CONFIG_HOME/emacs/bin" ];
    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];
  };
}
