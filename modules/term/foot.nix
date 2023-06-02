{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.term.foot;
  colors = config.modules.theme.color;
in {
  options.modules.term.foot = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    user.packages = with pkgs; [ foot ];
    home.configFile = {
      "foot/foot.ini".text = with colors;
        concatStrings [
          (concatMapStringsSep "\n" readFile
            [ "${configDir}/foot/foot.ini" ])
          (''
[colors]
alpha=1.0
background=${background}
foreground=${foreground}

## Normal/regular colors (color palette 0-7)
regular0=${color0}  # black
regular1=${color1}  # red
regular2=${color2}  # green
regular3=${color3}  # yellow
regular4=${color4}  # blue
regular5=${color5}  # magenta
regular6=${color6}  # cyan
regular7=${color7}  # white

## Bright colors (color palette 8-15)
bright0=${color8}   # bright black
bright1=${color9}   # bright red
bright2=${color10}   # bright green
bright3=${color11}   # bright yellow
bright4=${color12}   # bright blue
bright5=${color13}   # bright magenta
bright6=${color14}   # bright cyan
bright7=${color15}   # bright white

## dimmed colors (see foot.ini(5) man page)
dim0=${dim.black}
dim1=${dim.red}
dim2=${dim.green}
dim3=${dim.yellow}
dim4=${dim.blue}
dim5=${dim.magenta}
dim6=${dim.cyan}
dim7=${dim.white}

## The remaining 256-color palette
# 16 = <256-color palette #16>
# ...
# 255 = <256-color palette #255>

## Misc colors
# selection-foreground=<inverse foreground/background>
# selection-background=<inverse foreground/background>
# jump-labels=<regular0> <regular3>          # black-on-yellow
# scrollback-indicator=<regular0> <bright4>  # black-on-bright-blue
# search-box-no-match=<regular0> <regular1>  # black-on-red
# search-box-match=<regular0> <regular3>     # black-on-yellow
# urls=<regular3>
          '')
        ];
    };
  };
}
