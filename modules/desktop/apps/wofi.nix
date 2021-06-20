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
            margin: 0px;
            border-width: 0px;
            border-radius: 6px;
        }

        #input {
            color: #${color6};
            padding: 8px;
            background-color: #${color0};
            border-width: 1px;
            border-radius:  6px 6px 0px 0px;
            border-color: #${color10};
        }

        #inner-box {
          border-radius: 0px 0px 6px 6px;
          border-color: #${color10};
          border-width: 0px 1px 1px 1px;
          background-color: rgba(46,52,64,0.9);
        }

        #outer-box {
          border: none;
        }

        #scroll {
          margin: 0px;
          border: none;
        }

        #text {
          color: #${color0};
          padding: 5px;
          border: none;
        }

        button {
            padding: 6px;
            color: #${foreground};
            border-width: 2px 0px 2px 2px;
            border-radius: 4px 0px 0px 4px;
            border-color: #${foreground};
        }

        button selected normal {
            border-width: 2px 0px 2px 2px;
            border-color: #${foreground};
        }
      '';
    };
  };
}
