{ config, lib, pkgs, ... }:
# lock for wayland

with lib;
with lib.my;
let
  colors = config.modules.theme.color;
  cfg = config.modules.wayland.swaylock;
  lock = with pkgs; (writeScriptBin "lock" ''
    #!${stdenv.shell}
    ${playerctl}/bin/playerctl pause &
    ${swaylock-effects}/bin/swaylock
  '');
  screenOff = with pkgs; (writeScriptBin "screenOff" ''
    #!${stdenv.shell}
    swaymsg "output * dpms off"
  '');
  screenOn = with pkgs; (writeScriptBin "screenOn" ''
    #!${stdenv.shell}
    swaymsg "output * dpms off"
  '');
in {
  options.modules.wayland.swaylock = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    systemd.user.services.swayidle = {
      enable = true;
      description = "Idle Manager for Wayland";
      documentation = [ "man:swayidle(1)" ];
      wantedBy = [ "sway-session.target" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = ''
            ${pkgs.swayidle}/bin/swayidle \
              timeout 300 '${lock}/bin/lock' \
              timeout 600 '${screenOff}/bin/screenOff' \
              resume '${screenOn}/bin/screenOn' \
              before-sleep '${lock}/bin/lock'
         '';
      };
    };

    user.packages = with pkgs; [ lock swayidle screenOff screenOn ];
    home.configFile = {
      "swaylock/config".text = (with colors; ''
        line-color=${colors.background}
        inside-color=${colors.background}
        ring-color=${background}
        separator-color=${colors.foreground}
        key-hl-color=${colors.foreground}

        line-wrong-color=${colors.background}
        inside-wrong-color=${colors.urgent}
        ring-wrong-color=${colors.urgent}

        line-ver-color=${colors.background}
        inside-ver-color=${colors.background}
        ring-ver-color=${colors.background}

        grace=30
        screenshots
        fade-in=0.15
        effect-pixelate=20
        indicator-radius=50
      '');
    };
  };
}
