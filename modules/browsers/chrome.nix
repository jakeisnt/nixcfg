{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.browsers.chrome;
    chrome = pkgs.google-chrome;
in {
  options.modules.browsers.chrome = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    modules.browsers.chromium-help.enable = true;
    # The profiles here correspond to profiles created dynamically when opening Chrome and signing in.
    user.packages = with pkgs; [ chrome ];
  };
}
