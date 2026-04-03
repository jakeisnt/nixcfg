{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.term.ghostty;
in {
  options.modules.term.ghostty = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    user.packages = with pkgs; [ ghostty ];
    home.configFile = {
      "ghostty/config".source = mkOutOfStoreSymlink "${configDir}/ghostty/config";
    };
  };
}
