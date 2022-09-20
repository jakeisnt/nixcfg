# Emacs is my main driver. I'm the author of Doom Emacs
# https://github.com/hlissner/doom-emacs. This module sets it up to meet my
# particular Doomy needs.

{ config, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
# emacs pgtk + native-comp
# this was `29.2`, but we're having some issues...
let
  myemacs0 = pkgs.emacsNativeComp;
  emacsWPkgs = (pkgs.emacsPackagesFor myemacs0).emacsWithPackages;
  myemacs = emacsWPkgs (epkgs: (with epkgs; [ vterm pdf-tools org-pdftools ]));

  cfg = config.modules.editors.emacs;
  # install doom emacs if not already configured!!
  # ensure we have the write things in system path too!
  # yes this file name is legal, yes it is bad, but yes it lets us use the emacs service
  daemonScript = pkgs.writeScriptBin "emacs" ''
    #!${pkgs.bash}/bin/bash -l
    export PATH=$PATH:${lib.makeBinPath [ pkgs.git pkgs.sqlite pkgs.unzip ]}
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
      install = true;
      package = daemonScript;
      defaultEditor = false; # configured elsewhere
    };

    user.packages = with pkgs; [
      ## Emacs itself
      binutils # native-comp needs 'as', provided by this
      myemacs # 28 + pgtk + native-comp
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
    ];

    env.PATH = [ "$XDG_CONFIG_HOME/emacs/bin" ];
    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];
  };
}
