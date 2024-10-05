{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.term.alacritty;
  colors = config.modules.theme.color;
in {
  options.modules.term.alacritty = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    user.packages = with pkgs; [ alacritty ];
    home.configFile = {
      "alacritty/alacritty.toml".text = with colors;
        concatStrings [
          (concatMapStringsSep "\n" readFile
            [ "${configDir}/alacritty/alacritty.toml" ])
        ];
    };
  };
}
