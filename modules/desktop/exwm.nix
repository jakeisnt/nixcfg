{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.exwm;
in {
  options.modules.desktop.exwm = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      displayManager = {
        defaultSession = "none+exwm";
      };

      windowManager.session = lib.singleton {
        name = "exwm";
        start = ''
          emacs --daemon -f exwm-enable
          emacsclient -c
        '';
      };
    };
  };
}
