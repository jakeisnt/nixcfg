{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.browsers.brave;
in {
  options.modules.desktop.browsers.brave = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    modules.desktop.browsers.chromium-help.enable = true;
    user.packages = with pkgs; [ brave ];
  };
}
