{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.theme;
  colorscheme = {
    black = "504945";
    red = "ea5252";
    green = "ACB765";
    yellow = "c1bf89";
    blue = "82abbc";
    magenta = "b18a97";
    cyan = "88b482";
    white = "bfddb2";
  };
in {
  config = mkIf (cfg.active == "serenade") (mkMerge [
    # Desktop-agnostic configuration
    {
      modules = {
        theme = {
          gtk = {
            theme = "Nordic";
            iconTheme = "Zafiro-icons";
            cursorTheme = "Numix-Cursor";
          };
          color = with colorscheme; {
            foreground = white;
            background = "2a2f33";
            fadeColor = "2a2f33";
            fgAlt = "f8f8f2";
            urgent = "FF5555";
            bgAlt = "";

            color0 = black; 
            color1 = red;
            color2 = green;
            color3 = yellow;
            color4 = blue;
            color5 = magenta;
            color6 = cyan;
            color7 = white;
            color8 = black; # black2
            color9 = red;
            color10 = green;
            color11 = yellow;
            color12 = blue;
            color13 = magenta;
            color14 = cyan;
            color15 = white;
            dim = {
              black = black;
              red = red;
              green = green;
              yellow = yellow;
              blue = blue;
              magenta = magenta;
              cyan = cyan;
              white = white;
            };
          };
        };

        shell.zsh.rcFiles = [ ./config/zsh/prompt.zsh ];
        shell.tmux.rcFiles = [ ./config/tmux.conf ];
        desktop.browsers = {
          firefox.userChrome = concatMapStringsSep "\n" readFile
          [ ./config/firefox/userChrome.css ];
        };
      };
    }
    # Desktop theming
    {
      user.packages = with pkgs; [ zafiro-icons numix-cursor-theme ];
      fonts = {
        fonts = with pkgs; [
          fira-code
          fira-code-symbols
          jetbrains-mono
          siji
          font-awesome-ttf
        ];
        fontconfig.defaultFonts = {
          sansSerif = [ "Fira Sans" ];
          monospace = [ "Fira Code" ];
        };
      };

      home.configFile = with config.modules;
      mkMerge [
        (mkIf desktop.media.graphics.vector.enable {
          "inkscape/templates/default.svg".source =
          ./config/inkscape/default-template.svg;
          })
          (mkIf editors.vim.enable {
          "nvim/theme.vim" = { source = ./config/theme.vim; };
          })
          ];
          }
          ]);
          }
