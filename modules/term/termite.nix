{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.term.termite;
in {
    options.modules.term.termite = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    user.packages = with pkgs; [ termite ];
    home.configFile = { "termite" = { source = "${configDir}/termite"; }; };
  };
}
