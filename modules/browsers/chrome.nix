{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.browsers.chrome;
in {
  options.modules.browsers.chrome = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    modules.browsers.chromium-help.enable = true;
    # The profiles here correspond to profiles created dynamically when opening Chrome and signing in.
    user.packages = with pkgs; [
      google-chrome-dev
      (makeDesktopItem {
        name = "chrome-improvin";
        desktopName = "Chrome (Improvin)";
        genericName = "Open a Chrome window to the Improvin workspace";
        icon = "google-chrome-unstable";
        exec = "${google-chrome-dev}/bin/google-chrome-unstable --profile-directory=\"Profile 2\"";
        categories = ["Network"];
      })
      (makeDesktopItem {
        name = "chrome-default";
        desktopName = "Chrome (Default)";
        genericName = "Open a Chrome window to the default workspace";
        icon = "google-chrome-unstable";
        exec = "${google-chrome-dev}/bin/google-chrome-unstable --profile-directory=Default";
        categories = ["Network"];
      })
    ];
  };
}
