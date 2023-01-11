{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.sway;
  colors = config.modules.theme.color;
in {
  options.modules.desktop.sway = {
    enable = mkBoolOpt false;        # practical and basic
    fancy = mkBoolOpt false;         # fancy and pretty config
    disable-touch = mkBoolOpt false; # disable touchpad, touchscreen
    scale = mkOpt lib.types.float 1.0;
  };

  config = mkIf cfg.enable {
    modules.wayland.enable = true;
    modules.wayland.mako.enable = true;
    modules.wayland.swaylock.enable = cfg.fancy;
    modules.wayland.waybar.enable = cfg.fancy;
    modules.wayland.kanshi.enable = false;

    user.extraGroups = ["sway"];

    programs.sway = {
      enable = true;
      extraPackages = with pkgs;
        [ ] ++ (if cfg.fancy then [ autotiling ] else [ ]);
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

    modules.shell.loginInit = ''
      if [ -z $DISPLAY ] && [ $XDG_VTNR -eq 1 ]; then
        exec sway
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
            set $bgalt #${bgAlt}
            set $urgenttext #${fgAlt}
            set $inactiveback #${bgAlt2}
            set $pholdback #${color9}
            set $focusedback #${color8}

            set $mon2 DP-2
            set $term ${pkgs.alacritty}/bin/alacritty

            output eDP1 resolution 3840x2160 position 0,0
            output DP-2 resolution 1920x1080 scale 0.8

            exec 'swaymsg output eDP-1 scale ${lib.strings.floatToString cfg.scale}'
          '')
          (concatMapStringsSep "\n" readFile [ "${configDir}/sway/config" ])
          (if config.modules.hardware.audio.enable then with pkgs; ''
            bindsym XF86AudioRaiseVolume exec '${pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ +5%'
            bindsym XF86AudioLowerVolume exec '${pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ -5%'
            bindsym XF86AudioMute exec '${pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle'
          '' else "")
          (if cfg.disable-touch then
            ''
            input type:touch {
            dwt disabled
            tap disabled
            drag disabled
            events disabled
            }

            input type:touchpad {
            dwt disabled
            tap disabled
            drag disabled
            events disabled
            }
            '' else ""
          )
          (if cfg.fancy then

          ''
            gaps outer 5
            gaps inner 10

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
