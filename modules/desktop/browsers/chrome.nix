{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.browsers.chrome;
in {
  options.modules.desktop.browsers.chrome = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    modules.desktop.browsers.chromium-help.enable = true;
    user.packages = with pkgs; [ google-chrome-dev ];
  };
}
