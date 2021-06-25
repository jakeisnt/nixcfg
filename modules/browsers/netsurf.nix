{ options, config, lib, pkgs, ... }:

# a JavaScript-free web browser.
with lib;
with lib.my;
let cfg = config.modules.browsers.netsurf;
in {
  options.modules.browsers.netsurf = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable { user.packages = with pkgs; [ netsurf-browser ]; };
}
