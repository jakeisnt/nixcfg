{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.browsers.chrome;
in {
  options.modules.browsers.chrome = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    modules.browsers.chromium-help.enable = true;
    user.packages = with pkgs; [ google-chrome-dev ];
  };
}
