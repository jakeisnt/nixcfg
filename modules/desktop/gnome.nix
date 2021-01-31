{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.gnome;
  colors = config.modules.theme.color;
in {
  options.modules.desktop.gnome = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services = {
      xserver = {
        enable = true;
        dpi = 200;
        layout = "us";
        xkbOptions = "caps:swapescape";
        libinput.enable = true; # enable touchpad
        displayManager.gdm.enable = true;
        desktopManager.gnome3.enable = true;

        # todo: device-specific
        monitorSection = ''
          DisplaySize 508 285
        '';
      };
    };
  };
}
