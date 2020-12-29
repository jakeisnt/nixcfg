{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.term.termite;
in {
  options.modules.desktop.term.termite = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    user.packages = with pkgs; [ termite ];
    home.configFile = { "termite" = { source = "${configDir}/termite"; }; };
  };
}
