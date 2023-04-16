{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.theme;
  colorscheme = {
    stilla0 = "0D0D0D";
    stilla1 = "121414";
    stilla2 = "1A1C1C";
    stilla3 = "4C566A";
    stilla4 = "F2F2F2";
    stilla5 = "FAFAFA";
    stilla6 = "FAF5EF";
    stilla7 = "8FBCBB";
    stilla8 = "88B6D0";
    stilla9 = "ADB2BA";
    stilla10 = "5E81AC";
    stilla11 = "BA8082";
    stilla12 = "d99962";
    stilla13 = "E9B872";
    stilla14 = "A19C9A";
    stilla15 = "CD96B3";
  };
in {
  config = mkIf (cfg.active == "stilla") (mkMerge [
    # Desktop-agnostic configuration
    {
      modules = {
        theme = {
          gtk = {
            theme = "Yaru";
            iconTheme = "Zafiro-icons";
            cursorTheme = "openzone-cursors";
          };
          color = with colorscheme; {
            foreground = stilla4;
            background = stilla0;
            fadeColor = stilla3;
            fgAlt = stilla6;
            bgAlt = stilla1;
            bgAlt2 = stilla2;
            color0 = stilla1;
            color1 = stilla11;
            color2 = stilla14;
            color3 = stilla13;
            color4 = stilla9;
            color5 = stilla15;
            color6 = stilla8;
            color7 = stilla5;
            color8 = stilla3;
            color9 = stilla11;
            color10 = stilla14;
            color11 = stilla13;
            color12 = stilla9;
            color13 = stilla15;
            color14 = stilla7;
            color15 = stilla6;
            normal = {
              black = stilla1;
              red = stilla11;
              green = stilla14;
              yellow = stilla13;
              blue = stilla9;
              magenta = stilla15;
              cyan = stilla8;
              white = stilla5;
            };
            alt = {
              black = stilla3;
              red = stilla11;
              green = stilla14;
              yellow = stilla13;
              blue = stilla9;
              magenta = stilla15;
              cyan = stilla7;
              white = stilla6;
            };
            dim = {
              black = "373e4d";
              red = "94545d";
              green = "809575";
              yellow = "b29e75";
              blue = "68809a";
              magenta = "8c738c";
              cyan = "6d96a5";
              white = "aeb3bb";
            };
            urgent = "FF5555";
          };
        };
        shell.tmux.rcFiles = [ ./config/tmux.conf ];
        browsers = {
          firefox.userChrome = concatMapStringsSep "\n" readFile
            [ ./config/firefox/userChrome.css ];
        };
      };
    }

    # Desktop theming
    {
      user.packages = with pkgs;
        [ yaru-theme zafiro-icons openzone-cursors ];
        # ++ (if config.modules.media.spotify.enable then
        #   [ extras.spicetify-nix.packages.x86_64-linux.stilla ]
        # else
          # [ ]);
      fonts = {
        fonts = with pkgs; [
          fira-code
          fira-code-symbols
          jetbrains-mono
          siji
          font-awesome
          roboto-mono
          ibm-plex
        ];
        fontconfig.defaultFonts = {
          sansSerif = [ "Fira Sans" ];
          monospace = [ "BerkeleyMono_Regular" ]; # was fira code
        };
      };

      home.configFile = mkIf config.modules.editors.vim.enable {
        "nvim/theme.vim" = { text = "colorscheme stilla"; };
      };
    }
  ]);
}
