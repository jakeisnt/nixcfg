{ config, lib, pkgs, ... }:
# notifications for wayland

with lib;
with lib.my;
let
  colors = config.modules.theme.color;
  cfg = config.modules.wayland.mako;
in {
  options.modules.wayland.mako = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ mako ];
    home.configFile = {
      "mako/config".text = with colors; ''
        sort=-time
        layer=overlay
        max-visible=-1
        background-color=#${background}
        border-color=#${color0}
        text-color=#${foreground}
        width=300
        height=110
        border-size=1
        default-timeout=5000
        ignore-timeout=1
        margin=10,12

        [urgency=low]
        background-color=#${background}
        border-color=#${color0}

        [urgency=normal]
        background-color=#${background}
        border-color=#${color0}

        [urgency=high]
        background-color=#${urgent}
        border-color=#${urgent}
        default-timeout=0

        [category=mpd]
        default-timeout=2000
        group-by=category

        [category=spotify]
        default-timeout=2000
        group-by=category
      '';
    };
  };
}
