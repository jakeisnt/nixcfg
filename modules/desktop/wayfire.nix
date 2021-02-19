{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.wayfire;
  colors = config.modules.theme.color;
  startwayfire = (pkgs.writeTextFile {
    name = "startwayfire";
    destination = "/bin/startwayfire";
    executable = true;
    text = ''
      #! ${pkgs.bash}/bin/bash

      # first import environment variables from the login manager
      systemctl --user import-environment
      # then start the service
      exec systemctl --user start wayfire.service
    '';
  });
  latitude = "45.5";
  longitude = "-122.65";
in {
  options.modules.desktop.wayfire = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    env.XDG_SESSION_TYPE = "wayland";
    modules.desktop.apps.wofi.enable = true;

    user.packages = with pkgs; [
      wayfire
      xwayland
      mako
      kanshi
      wl-clipboard
      sway-contrib.grimshot
      wf-recorder
      swayidle
      swaylock
      # due to overlay,
      # these are now wayland clipboard interoperable
      xclip
      xsel
    ];

    env.XDG_CURRENT_DESKTOP = "wayfire";

    environment.systemPackages = with pkgs; [ startwayfire ];
    systemd.user.targets.wayfire-session = {
      description = "Wayfire compositor session";
      documentation = [ "man:systemd.special(7)" ];
      bindsTo = [ "graphical-session.target" ];
      wants = [ "graphical-session-pre.target" ];
      after = [ "graphical-session-pre.target" ];
    };

    systemd.user.services.wayfire = {
      description = "Wayfire - Wayland window manager";
      documentation = [ "man:wayfire(5)" ];
      bindsTo = [ "graphical-session.target" ];
      wants = [ "graphical-session-pre.target" ];
      after = [ "graphical-session-pre.target" ];
      # We explicitly unset PATH here, as we want it to be set by
      # systemctl --user import-environment in startsway
      environment.PATH = lib.mkForce null;
      serviceConfig = {
        Type = "simple";
        ExecStart = ''
          ${pkgs.dbus}/bin/dbus-run-session ${pkgs.wayfire}/bin/wayfire --debug
        '';
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
    };

    # systemd.user.services.wlsunset = mkIf cfg.fancy {
    #   description = "Red screen for Wayland";
    #   documentation = [ "man:wlsunet(1)" ];
    #   wantedBy = [ "sway-session.target" ];
    #   partOf = [ "graphical-session.target" ];
    #   serviceConfig = {
    #     ExecStart =
    #       "${pkgs.wlsunset}/bin/wlsunset -l ${latitude} -L ${longitude}";
    #   };
    # };

    modules.shell.zsh.rcInit = ''
      if [ -z $DISPLAY ] && [ "$(tty)" == "/dev/tty1" ]; then 
        exec startwayfire
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
            @define-color foreground #4d4d4d;
            @define-color background #282a36;
            @define-color fgalt #${fgAlt};
            @define-color bgalt #e6e6e6;
            @define-color cyan #ff6e67;
            @define-color green #5af78e;
            @define-color yellow #f1fa8c;
            @define-color blue #9aedfe;
            @define-color purple #bd93f9;
            @define-color buttonhover #ff79c6;
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
