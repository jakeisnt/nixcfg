{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.xfce;
  colors = config.modules.theme.color;
in {
  options.modules.desktop.xfce = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services = {
      xserver = {
        enable = true;
        xkbOptions = "caps:swapescape";
        desktopManager.xfce.enable = true;
        desktopManager.xterm.enable = true;
        displayManager.defaultSession = "xfce";

        monitorSection = ''
          DisplaySize 508 285
        '';
      };
    };
  };
}
