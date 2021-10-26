# Emacs is my main driver. I'm the author of Doom Emacs
# https://github.com/hlissner/doom-emacs. This module sets it up to meet my
# particular Doomy needs.

{ config, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let
  myemacs = pkgs.emacsPgtkGcc;
  cfg = config.modules.editors.emacs;
  # from https://github.com/Mic92/dotfiles
  treeSitterGrammars = pkgs.runCommandLocal "grammars" {} ''
    mkdir -p $out/bin
    ${lib.concatStringsSep "\n"
      (lib.mapAttrsToList (name: src: "ln -s ${src}/parser $out/bin/${name}.so") pkgs.tree-sitter.builtGrammars)};
  '';

  # install doom emacs if not already configured!!
  # ensure we have the write things in system path too!
  # yes this file name is legal, yes it is bad, but yes it lets us use the emacs service
  daemonScript = pkgs.writeScriptBin "emacs" ''
    #!${pkgs.zsh}/bin/zsh -l
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

  langs = [
    "agda"
    "bash"
    "c"
    "c-sharp"
    "cpp"
    "css"
    /*"elm" */
    "fluent"
    "go"
    /*"hcl"*/
    "html"
    /*"janet-simple"*/
    "java"
    "javascript"
    "jsdoc"
    "json"
    "ocaml"
    "python"
    "php"
    /*"pgn"*/
    "ruby"
    "rust"
    "scala"
    "swift"
    "typescript"
  ];

  grammars = lib.getAttrs (map (lang: "tree-sitter-${lang}") langs) pkgs.tree-sitter.builtGrammars;

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

    # systemd.user.services.emacs = {
    #   description = "Emacs: the extensible, self-documenting text editor";

    #   serviceConfig = {
    #     Type = "forking";
    #     ExecStart = "${pkgs.bash}/bin/bash -c 'source ${config.system.build.setEnvironment}; exec ${daemonScript}'";
    #     ExecStop = "${myemacs}/bin/emacsclient --eval (kill-emacs)";
    #     Restart = "always";
    #   };
    #   wantedBy = [ "default.target" ];
    # };

    # systemd.user.services.emacs-daemon = mkIf cfg.daemon {
    #   enable = true;
    #   serviceConfig = {
    #     Install.WantedBy = [ "default.target" ];
    #     Type = "forking";
    #     TimeoutStartSec = "10min";
    #     Restart = "always";
    #     ExecStart = toString daemonScript;
    #   };
    # };

    services.emacs = mkIf cfg.daemon {
      enable = true;
      install = true;
      package = daemonScript;
      # defaultEditor = false; # configured elsewhere
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
      # better syntax highlighting
      tree-sitter

      # for vterm
      libvterm
      libtool

      # for org-roam graph
      graphviz

      pinentry-emacs
    ];

    home.file.".tree-sitter".source = (pkgs.runCommand "grammars" {} ''
      mkdir -p $out/bin
      ${lib.concatStringsSep "\n"
        (lib.mapAttrsToList (name: src: "name=${name}; ln -s ${src}/parser $out/bin/\${name#tree-sitter-}.so") grammars)};
    '');

    env.PATH = [ "$XDG_CONFIG_HOME/emacs/bin" ];
    modules.shell.zsh.rcFiles = [ "${configDir}/emacs/aliases.zsh" ];

    # no vim allowed
    environment.shellAliases = {
      vim = "emacs";
      vi = "emacs";
      v = "emacs";
    };

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];
  };
}
