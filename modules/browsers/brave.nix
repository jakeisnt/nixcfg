{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.browsers.brave;
in {
  options.modules.browsers.brave = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    modules.browsers.chromium-help.enable = true;
    user.packages = with pkgs; [ brave ];
  };
}
