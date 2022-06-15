# modules/dev/clojure.nix --- https://clojure.org/
#
# I use clojure now > : )

{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.clojure;
in {
  options.modules.dev.clojure = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ jdk17 clojure-lsp clojure joker leiningen ];
  };
}
