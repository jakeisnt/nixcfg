{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.term.alacritty;
  colors = config.modules.theme.color;
in {
  options.modules.desktop.term.alacritty = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    user.packages = with pkgs; [ alacritty ];
    home.configFile = {
      "alacritty/alacritty.yml".text = with colors;
        concatStrings [
          (concatMapStringsSep "\n" readFile
            [ "${configDir}/alacritty/alacritty.yml" ])
          (''
            colors:
              primary:
                background: '#${background}'
                foreground: '#${foreground}'
                dim_foreground: '#${fadeColor}'
              cursor:
                text: '#${background}'
                cursor: '#${foreground}'
              vi_mode_cursor:
                text: '#${background}'
                cursor: '#${foreground}'
              selection:
                text: CellForeground
                background: '#${fadeColor}'
              search:
                matches:
                  foreground: CellBackground
                  background: '#${color6}'
                # bar:
                #   background: '#434c5e'
                #   foreground: '#${foreground}'
              normal:
                black: '#${color0}'
                red: '#${color1}'
                green: '#${color2}'
                yellow: '#${color3}'
                blue: '#${color4}'
                magenta: '#${color5}'
                cyan: '#${color6}'
                white: '#${color7}'
              bright:
                black: '#${color8}'
                red: '#${color9}'
                green: '#${color10}'
                yellow: '#${color11}'
                blue: '#${color12}'
                magenta: '#${color13}'
                cyan: '#${color14}'
                white: '#${color15}'
              dim:
                black: '#${dim.black}'
                red: '#${dim.red}'
                green: '#${dim.green}'
                yellow: '#${dim.yellow}'
                blue: '#${dim.blue}'
                magenta: '#${dim.magenta}'
                cyan: '#${dim.cyan}'
                white: '#${dim.white}'
          '')
        ];
    };
  };
}
