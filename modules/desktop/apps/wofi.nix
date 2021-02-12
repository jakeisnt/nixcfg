{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.apps.wofi;
  colors = config.modules.theme.color;
in {
  options.modules.desktop.apps.wofi = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      wofi
      (writeScriptBin "launch" ''
        #!${stdenv.shell}
        exec ${wofi}/bin/wofi --modi drun --show drun
      '')
    ];

    home.configFile = {
      "wofi/config".text = ''
        width=30%
        lines=6
        allow_images=true
        insensitive=true
      '';
      "wofi/style.css".text = with colors; ''
        window {
            location: center;
            anchor:   center;
            padding: 10px;
            border:  0px;
            border-radius: 6px;

            spacing: 0;
            children:  [mainbox];
            orientation: horizontal;
        }

        #input {
            color: #${color6};
            padding: 11px;
            background-color: #${color0};

            border: 1px;
            border-radius:  6px 6px 0px 0px;
            border-color: #${color10};
        }

        #inner-box {
          padding: 8px;
          border-radius: 0px 0px 6px 6px;
          border-color: #${color10};
          border: 0px 1px 1px 1px;
          background-color: rgba(46,52,64,0.9);
          dynamic: false;
        }

        #outer-box {
        }

        #scroll {
          display: none;
        }

        #text {
            color: ${color0};
            padding: 5;
            border-color: ${foreground};
            border:  0px 2px 2px 2px;
            background-color: ${color7};
        }

        button {
            padding: 6px;
            color: #${foreground};
            horizontal-align: 0.5;

            border: 2px 0px 2px 2px;
            border-radius: 4px 0px 0px 4px;
            border-color: #${foreground};
        }

        button selected normal {
            border: 2px 0px 2px 2px;
            border-color: ${foreground};
        }
      '';
    };
  };
}
