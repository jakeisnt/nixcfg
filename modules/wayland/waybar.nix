{ config, lib, pkgs, ... }:
# notifications for wayland

with lib;
with lib.my;
let
  colors = config.modules.theme.color;
  cfg = config.modules.wayland.waybar;
in {
  options.modules.wayland.waybar = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      [
        (if config.modules.hardware.audio.enable then
          waybar.override { pulseSupport = true; }
        else
          waybar)
      ];
    home.configFile = {
      "waybar/config" = { source = "${configDir}/waybar/config"; };

      # waybar inspiration credit goes to github.com/jakehamilton!
      "waybar/style.css".text = (with colors;
        concatStrings [
          ''
            @define-color foreground #${fgAlt};
            @define-color background #${background};
            @define-color buttonhover #${urgent};
            @define-color fgalt #${fgAlt};
            @define-color bgalt #${bgAlt2};
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
    };
  };
}
