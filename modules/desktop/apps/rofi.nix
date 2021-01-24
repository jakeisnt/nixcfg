{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.apps.rofi;
  colors = config.modules.theme.color;
in {
  options.modules.desktop.apps.rofi = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      rofi
      (writeScriptBin "launch" ''
        #!${stdenv.shell}
        exec ${rofi}/bin/rofi -modi drun -show drun -hide-scrollbar=true
      '')
      # For rapidly test changes to rofi's stylesheets
      # (writeScriptBin "rofi-test" ''
      #   #!${stdenv.shell}
      #   themefile=$1
      #   themename=${my.theme.name}
      #   shift
      #   exec rofi \
      #        -theme ${themesDir}/$themename/rofi/$themefile \
      #        "$@"
      #   '')
      # Fake rofi dmenu entries
      (makeDesktopItem {
        name = "rofi-bookmarkmenu";
        desktopName = "Open Bookmark in Browser";
        icon = "bookmark-new-symbolic";
        exec = "${binDir}/rofi/bookmarkmenu";
      })
      (makeDesktopItem {
        name = "rofi-filemenu";
        desktopName = "Open Directory in Terminal";
        icon = "folder";
        exec = "${binDir}/rofi/filemenu";
      })
      (makeDesktopItem {
        name = "rofi-filemenu-scratch";
        desktopName = "Open Directory in Scratch Terminal";
        icon = "folder";
        exec = "${binDir}/rofi/filemenu -x";
      })

      (makeDesktopItem {
        name = "lock-display";
        desktopName = "Lock screen";
        icon = "system-lock-screen";
        exec = "${binDir}/zzz";
      })
    ];

    #-show run -lines 5 -bw 2 -width 20 -padding 50 -eh 2 -hide-scrollbar true
    home.configFile = {
      "rofi/config".text = ''
        configuration {
          width: 30;
          lines: 6;
          columns: 2;
          font: "Envy Code R 10";
          show-icons: true;
          line-margin: 10;
          theme: "nord";
          display-window: "";
          display-run: "";
          display-ssh: "";
          display-drun: "";
          display-combi: "";
        }
      '';
      # https://github.com/undiabler/nord-rofi-theme
      "rofi/nord.rasi".text = with colors; ''
        /**
         * Nordic rofi theme
         * Adapted by undiabler <undiabler@gmail.com>
         *
         * Nord Color palette imported from https://www.nordtheme.com/
         *
         */

        configuration {
            font: "Envy Code R 10";
            width: 30;
            line-margin: 10;
            lines: 6;
            columns: 2;

            display-ssh:    "";
            display-run:    "";
            display-drun:   "";
            display-window: "";
            display-combi:  "";
            show-icons:     true;
        }

        * {
            nord0: #2e3440;
            nord1: #3b4252;
            nord2: #434c5e;
            nord3: #4c566a;

            nord4: #d8dee9;
            nord5: #e5e9f0;
            nord6: #eceff4;

            nord7: #8fbcbb;
            nord8: #88c0d0;
            nord9: #81a1c1;
            nord10: #5e81ac;
            nord11: #bf616a;

            nord12: #d08770;
            nord13: #ebcb8b;
            nord14: #a3be8c;
            nord15: #b48ead;

            foreground:  @nord9;
            backlight:   #ccffeedd;
            background-color:  transparent;

            highlight:     underline bold #eceff4;

            transparent: rgba(46,52,64,0);
        }

        window {
            location: center;
            anchor:   center;
            transparency: "screenshot";
            padding: 10px;
            border:  0px;
            border-radius: 6px;

            background-color: @transparent;
            spacing: 0;
            children:  [mainbox];
            orientation: horizontal;
        }

        mainbox {
            spacing: 0;
            children: [ inputbar, message, listview ];
        }

        message {
            color: @nord0;
            padding: 5;
            border-color: @foreground;
            border:  0px 2px 2px 2px;
            background-color: @nord7;
        }

        inputbar {
            color: @nord6;
            padding: 11px;
            background-color: #3b4252;

            border: 1px;
            border-radius:  6px 6px 0px 0px;
            border-color: @nord10;
        }

        entry, prompt, case-indicator {
            text-font: inherit;
            text-color:inherit;
        }

        prompt {
            margin: 0px 0.3em 0em 0em ;
        }

        listview {
            padding: 8px;
            border-radius: 0px 0px 6px 6px;
            border-color: @nord10;
            border: 0px 1px 1px 1px;
            background-color: rgba(46,52,64,0.9);
            dynamic: false;
        }

        element {
            padding: 3px;
            vertical-align: 0.5;
            border-radius: 4px;
            background-color: transparent;
            color: @foreground;
            text-color: rgb(216, 222, 233);
        }

        element selected.normal {
            background-color: @nord7;
            text-color: #2e3440;
        }

        button {
            padding: 6px;
            color: @foreground;
            horizontal-align: 0.5;

            border: 2px 0px 2px 2px;
            border-radius: 4px 0px 0px 4px;
            border-color: @foreground;
        }

        button selected normal {
            border: 2px 0px 2px 2px;
            border-color: @foreground;
        }
      '';
    };
  };
}
