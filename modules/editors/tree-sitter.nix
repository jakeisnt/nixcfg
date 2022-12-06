{ config, lib, pkgs, inputs, ... }:

with lib;
with lib.my;

let
    # from https://github.com/Mic92/dotfiles
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
# "php"
    /*"pgn"*/
    "ruby"
    "rust"
    "scala"
# "swift"
    "typescript"
    "org-nvim"
    "commonlisp"
];

    grammars = lib.getAttrs (map (lang: "tree-sitter-${lang}") langs) pkgs.tree-sitter.builtGrammars;
    cfg = config.modules.editors.tree-sitter;

    in {


        options.modules.editors.tree-sitter = {
            enable = mkBoolOpt false;
        };

        config = mkIf cfg.enable {
            user.packages = with pkgs; [
                tree-sitter
            ];

            home.file.".tree-sitter".source = (pkgs.runCommand "grammars" {} ''
        mkdir -p $out/bin
        ${lib.concatStringsSep "\n"
            (lib.mapAttrsToList (name: src: "name=${name}; ln -s ${src}/parser $out/bin/\${name#tree-sitter-}.so") grammars)};
'');
        };
    }
