{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.term.alacritty;
in {
  options.modules.term.alacritty = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    user.packages = with pkgs; [ alacritty ];
    home.configFile = {
      "alacritty/alacritty.toml".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/alacritty/alacritty.toml";
    };
  };
}
