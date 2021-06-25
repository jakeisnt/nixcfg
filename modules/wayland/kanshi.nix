{ config, lib, pkgs, ... }:
# notifications for wayland

with lib;
with lib.my;
let
  colors = config.modules.theme.color;
  cfg = config.modules.wayland.kanshi;
in {
  options.modules.wayland.kanshi = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ kanshi ];
    systemd.user.services.kanshi = {
      description = "Kanshi output autoconfig ";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        # kanshi doesn't have an option to specifiy config file yet, so it looks
        # at .config/kanshi/config
        ExecStart = ''
          ${pkgs.kanshi}/bin/kanshi
        '';
        RestartSec = 5;
        Restart = "always";
      };
    };
    home.configFile = {
      "kanshi/config" = {
        text = ''
          profile workdesk {
            output eDP-1 enable scale 2 mode 3840x2160 position 0,0
            output DP-2 enable scale 1.5
            exec ${pkgs.sway}/bin/swaymsg workspace 1, move workspace to DP-2
          }

          profile laptop {
            output eDP-1 enable mode 3840x2160 position 0,0
          }
        '';
      };
    };
  };
}
