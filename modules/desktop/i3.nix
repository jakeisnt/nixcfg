{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.i3;
  colors = config.modules.theme.color;
in {
  options.modules.desktop.i3 = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {

    services = {
      picom.enable = true;
      redshift = { enable = true; };
      xserver = {
        enable = true;
        layout = "us";
        xkbOptions = "caps:swapescape";
        libinput.enable = true; # enable touchpad
        displayManager.defaultSession = "none+i3";
        windowManager.i3 = {
          enable = true;
          extraPackages = with pkgs; [
            # notifications
            dunst
            libnotify
            dmenu
          ];
        };

        # todo: device-specific
        monitorSection = ''
          DisplaySize 508 285
        '';
      };
    };

    systemd.user.services."dunst" = {
      enable = true;
      description = "";
      wantedBy = [ "default.target" ];
      serviceConfig.Restart = "always";
      serviceConfig.RestartSec = 2;
      serviceConfig.ExecStart = "${pkgs.dunst}/bin/dunst";
    };

    env.XDG_CURRENT_DESKTOP = "i3";
    home.configFile = {
      "i3/config".text = with colors;
        concatStrings [
          (''
            set $foreground #${foreground}
            set $background #${background}
            set $lighterbg  #${fadeColor}
            set $urgent #${urgent}
            set $urgenttext #F8F8F2
            set $inactiveback #44475A
            set $pholdback #282A36
            set $focusedback #6f757d
          '')
          (concatMapStringsSep "\n" readFile [ "${configDir}/i3/config" ])
        ];
      "dunst/config".text = with colors; ''
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

      "picom/config".text = "";
    };
  };
}
