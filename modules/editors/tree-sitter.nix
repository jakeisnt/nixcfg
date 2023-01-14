{ config, lib, pkgs, inputs, ... }:

# Fetch and compile tree-sitter grammars ahead of time rather than with a stateful configuration.
# Inspired by https://github.com/Mic92/dotfiles.

with lib;
with lib.my;

let
    treeSitterGrammars = pkgs.runCommandLocal "grammars" {} ''
mkdir -p $out/bin
${lib.concatStringsSep "\n"
    (lib.mapAttrsToList (name: src: "ln -s ${src}/parser $out/bin/${name}.so") pkgs.tree-sitter.builtGrammars)}
        '';
    langs = [
        "bash"
        "c"
        "c-sharp"
        "css"
        "go"
        "html"
        "java"
        "javascript"
        "jsdoc"
        "json"
        "ocaml"
        "python"
        "ruby"
        "rust"
        "scala"
        "typescript"
        "org-nvim"
        "clojure"
        "commonlisp"
    ];

    grammars = lib.getAttrs (map (lang: "tree-sitter-${lang}") langs) pkgs.tree-sitter.builtGrammars;
    cfg = config.modules.editors.tree-sitter;

in {
    options.modules.editors.tree-sitter = {
        enable = mkBoolOpt false;
    };

    config = mkIf cfg.enable {
        user.packages = with pkgs; [ tree-sitter ];
        home.file.".tree-sitter".source = treeSitterGrammars;
        home.configFile."tree-sitter" = {
            source = "${configDir}/tree-sitter";
            recursive = true; # include all utils, config files, etc...
        };
    };
}
