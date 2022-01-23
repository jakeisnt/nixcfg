{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.wayland.wofi;
  colors = config.modules.theme.color;
in {
  options.modules.wayland.wofi = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      wofi
      (writeScriptBin "launch" ''
        #!${stdenv.shell}
        exec ${wofi}/bin/wofi --show drun
      '')
    ];

    home.configFile = {
      "wofi/config".text = ''
        width=15%
        lines=5
        allow_images=false
        insensitive=true
        mode=drun
      '';
      "wofi/style.css".text = with colors; ''
        window {
            margin: 0px;
            border-width: 0px;
            border-radius: 0px;
        }

        #input {
            color: #${color4};
            padding: 8px;
            background-color: #${background};
            border-width: 1px 1px 1px 1px;
            border-color: #${color10};
        }

        #inner-box {
          border-color: #${color10};
          border-width: 0px 1px 1px 1px;
          background-color: #${background};
        }

        #outer-box {
          border-width: 0px 1px 1px 1px;
          background-color: #${background};
        }

        #scroll {
          margin: 0px;
          border: none;
        }

        #text {
          color: #${color4};
          padding: 5px;
          border: none;
        }

        button {
            padding: 6px;
            color: #${foreground};
            border-width: 2px 0px 2px 2px;
            border-color: #${foreground};
        }

        button selected normal {
            border-width: 0;
        }
      '';
    };
  };
}
