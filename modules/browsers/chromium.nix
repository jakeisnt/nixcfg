{ options, config, lib, pkgs, ... }:

# Chromium-based and other simple browser options.
# Shared chromium settings live in chromium-help.nix.
# Firefox has its own file due to complexity.

with lib;
with lib.my; {
  options.modules.browsers = {
    brave.enable = mkBoolOpt false;
    chrome.enable = mkBoolOpt false;
    netsurf.enable = mkBoolOpt false; # a JavaScript-free browser
  };

  config = mkMerge [
    (mkIf config.modules.browsers.brave.enable {
      modules.browsers.chromium-help.enable = true;
      user.packages = [ pkgs.brave ];
    })
    (mkIf config.modules.browsers.chrome.enable {
      modules.browsers.chromium-help.enable = true;
      user.packages = [ pkgs.google-chrome ];
    })
    (mkIf config.modules.browsers.netsurf.enable {
      user.packages = [ pkgs.netsurf-browser ];
    })
  ];
}
