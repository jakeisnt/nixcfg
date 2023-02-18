{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.browsers.chrome;
    chrome = pkgs.google-chrome-dev;
    chrome-improvin = pkgs.writeShellScriptBin "chrome-improvin" ''
      exec ${chrome}/bin/google-chrome-unstable --profile-directory="Profile 2" &
    '';
    chrome-default = pkgs.writeShellScriptBin "chrome-default" ''
      exec ${chrome}/bin/google-chrome-unstable --profile-directory="Default" &
    '';
in {
  options.modules.browsers.chrome = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    modules.browsers.chromium-help.enable = true;
    # The profiles here correspond to profiles created dynamically when opening Chrome and signing in.
    user.packages = with pkgs; [
      chrome
      chrome-improvin
      chrome-default
      (makeDesktopItem {
        name = "chrome-improvin";
        desktopName = "Chrome (Improvin)";
        genericName = "Open a Chrome window to the Improvin workspace";
        icon = "google-chrome-unstable";
        exec = "${chrome-improvin}/bin/chrome-improvin";
        categories = ["Network"];
      })
      (makeDesktopItem {
        name = "chrome-default";
        desktopName = "Chrome (Default)";
        genericName = "Open a Chrome window to the default workspace";
        icon = "google-chrome-unstable";
        exec = "${chrome-default}/bin/chrome-default";
        categories = ["Network"];
      })
    ];
  };
}
