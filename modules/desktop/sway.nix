{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.sway;
  colors = config.modules.theme.color;
  startsway = (pkgs.writeTextFile {
    name = "startsway";
    destination = "/bin/startsway";
    executable = true;
    text = ''
      #! ${pkgs.bash}/bin/bash

      # first import environment variables from the login manager
      systemctl --user import-environment
      # then start the service
      exec systemctl --user start sway.service
    '';
  });
  latitude = "45.5";
  longitude = "-122.65";
in {
  options.modules.desktop.sway = {
    enable = mkBoolOpt false; # practical and basic
    fancy = mkBoolOpt false; # fancy and pretty config
  };

  config = mkIf cfg.enable {
    modules.wayland.enable = true;
    modules.wayland.mako.enable = true;
    modules.wayland.swaylock.enable = cfg.fancy;
    modules.wayland.waybar.enable = cfg.fancy;
    modules.wayland.kanshi.enable = false;

    user.extraGroups = [ "sway" ];

    programs.sway = {
      enable = true;
      extraPackages = with pkgs;
        [ startsway ] ++ (if cfg.fancy then [ autotiling ] else [ ]);
      wrapperFeatures.gtk = true;
      extraSessionCommands = ''
        export SDL_VIDEODRIVER=wayland
        # needs qt5.qtwayland in systemPackages
        export QT_QPA_PLATFORM=wayland
        export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
        # Fix for some Java AWT applications (e.g. Android Studio),
        # use this if they aren't displayed properly:
        export _JAVA_AWT_WM_NONREPARENTING=1
      '';
    };

    env.XDG_CURRENT_DESKTOP = "sway";

    systemd.user.targets.sway-session = {
      enable = true;
      description = "Sway compositor session";
      documentation = [ "man:systemd.special(7)" ];

      bindsTo = [ "graphical-session.target" ];
      wants = [ "graphical-session-pre.target" ];
      after = [ "graphical-session-pre.target" ];
      requiredBy =
        [ "graphical-session.target" "graphical-session-pre.target" ];
    };

    systemd.user.services.sway = {
      enable = true;
      description = "Sway - Wayland window manager";
      documentation = [ "man:sway(5)" ];
      bindsTo = [ "graphical-session.target" ];
      wants = [ "graphical-session-pre.target" ];
      after = [ "graphical-session-pre.target" ];
      # We explicitly unset PATH here, as we want it to be set by
      # systemctl --user import-environment in startsway
      environment.PATH = lib.mkForce null;
      serviceConfig = {
        Type = "simple";
        ExecStart = ''
          ${pkgs.dbus}/bin/dbus-run-session ${pkgs.sway}/bin/sway
        '';
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
    };

    modules.shell.zsh.rcInit = ''
      if [ -z $DISPLAY ] && [ "$(tty)" == "/dev/tty1" ]; then
        startsway
      fi
    '';

    home.configFile = {
      "sway/config".text = with colors;
        concatStrings [
          (''
            set $foreground #${foreground}
            set $background #${background}
            set $lighterbg  #${fadeColor}
            set $urgent #${urgent}
            set $urgenttext #${fgAlt}
            set $inactiveback #44475A
            set $pholdback #282A36
            set $focusedback #6f757d

            output eDP1 resolution 3840x2160 position 0,0
            output DP-2 resolution 1920x1080 scale 0.8
          '')
          (concatMapStringsSep "\n" readFile [ "${configDir}/sway/config" ])
          (if cfg.fancy then

          ''
            gaps outer 8
            gaps inner 5

            bindsym $mod+d exec 'pkill -SIGUSR1 waybar'

            bar {
                swaybar_command ${pkgs.waybar}/bin/waybar
            }

            exec ${pkgs.autotiling}/bin/autotiling
          '' else ''
            bar {
              position bottom
              # When the status_command prints a new line to stdout, swaybar updates.
              # The default just shows the current date and time.
              status_command while date +'%Y-%m-%d %H:%M'; do sleep 1; done
              colors {
                background  $background
                statusline  $foreground
                separator   $background

                #Type               border      background  font
                focused_workspace   $lighterbg  $lighterbg  $foreground
                active_workspace    $background $background $foreground
                inactive_workspace  $background $background $foreground
                urgent_workspace    $background $background $foreground
              }
            }
          '')
        ];
    };
  };
}
