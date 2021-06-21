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
  lock = (pkgs.writeScriptBin "lock" ''
    #!${pkgs.stdenv.shell}
    exec ${pkgs.swaylock-effects}/bin/swaylock
  '');
in {
  options.modules.desktop.sway = {
    enable = mkBoolOpt false; # practical and basic
    fancy = mkBoolOpt false; # fancy and pretty config
  };

  config = mkIf cfg.enable {
    env.XDG_SESSION_TYPE = "wayland";
    modules.desktop.apps.wofi.enable = true;

    programs.sway = {
      enable = true;
      extraPackages = with pkgs;
        [
          startsway
          xwayland
          qt5.qtwayland
          mako
          kanshi
          wl-clipboard
          sway-contrib.grimshot
          wf-recorder

          # greeter
          greetd.tuigreet

          # due to overlay these are now wayland clipboard interoperable
          xclip
          xsel
        ] ++ (if cfg.fancy then [
          # extra
          (waybar.override { pulseSupport = true; })

          swaylock-effects
          swayidle
          lock
          autotiling
        ] else
          [ ]);
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
          ${pkgs.dbus}/bin/dbus-run-session ${pkgs.sway}/bin/sway --debug
        '';
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
    };

    systemd.user.services.swayidle = mkIf cfg.fancy {
      # enable = true;
      description = "Idle Manager for Wayland";
      documentation = [ "man:swayidle(1)" ];
      wantedBy = [ "sway-session.target" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = ''
          ${pkgs.swayidle}/bin/swayidle -w -d \
                   timeout 5 '${lock}/bin/lock && ${pkgs.sway}/bin/swaymsg "output * dpms off"' \
                   resume '${pkgs.sway}/bin/swaymsg "output * dpms on"'
                 '';
      };
    };

    services.greetd = {
      enable = true;
      restart = true;
      vt = 2;
      settings = {
        default_session = {
          command =
            "${pkgs.greetd.tuigreet}/bin/tuigreet --remember --asterisks --time --cmd ${startsway}/bin/startsway";
          user = "greeter";
        };
      };
    };

    security.pam.services.greetd.enableGnomeKeyring = true;

    modules.shell.zsh.rcInit = ''
      if [ -z $DISPLAY ] && [ "$(tty)" == "/dev/tty1" ]; then
        startsway
      fi
    '';

    home.configFile = {
      "swaylock/config".text = (mkIf cfg.fancy (with colors; ''
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
      ''));
      "waybar/config" =
        mkIf cfg.fancy { source = "${configDir}/waybar/config"; };

      # waybar inspiration credit goes to github.com/jakehamilton!
      "waybar/style.css".text = mkIf cfg.fancy (with colors;
        concatStrings [
          ''
            @define-color foreground #${fgAlt};
            @define-color background #${background};
            @define-color buttonhover #${urgent};
            @define-color fgalt #${fgAlt};
            @define-color bgalt #44475A;
            @define-color cyan #${normal.cyan};
            @define-color green #${normal.green};
            @define-color yellow #${normal.yellow};
            @define-color blue #${normal.blue};
            @define-color purple #${normal.magenta};
            @define-color cyanalt #${dim.cyan};
            @define-color greenalt #${dim.green};
            @define-color yellowalt #${dim.yellow};
            @define-color bluealt #${dim.blue};
            @define-color purplealt #${dim.magenta};
          ''
          (concatMapStringsSep "\n" readFile
            [ "${configDir}/waybar/style.css" ])
        ]);
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
