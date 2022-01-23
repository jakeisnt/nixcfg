{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.theme;
  colorscheme = {
    nord0 = "0D0D0D";
    nord1 = "121414";
    nord2 = "1A1C1C";
    nord3 = "4C566A";
    nord4 = "F2F2F2";
    nord5 = "FAFAFA";
    nord6 = "FAF5EF";
    nord7 = "8FBCBB";
    nord8 = "88B6D0";
    nord9 = "ADB2BA";
    nord10 = "5E81AC";
    nord11 = "BA8082";
    nord12 = "d99962";
    nord13 = "E9B872";
    nord14 = "A19C9A";
    nord15 = "CD96B3";
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
            foreground = nord4;
            background = nord0;
            fadeColor = nord3;
            fgAlt = nord6;
            bgAlt = nord1;
            bgAlt2 = nord2;
            color0 = nord1;
            color1 = nord11;
            color2 = nord14;
            color3 = nord13;
            color4 = nord9;
            color5 = nord15;
            color6 = nord8;
            color7 = nord5;
            color8 = nord3;
            color9 = nord11;
            color10 = nord14;
            color11 = nord13;
            color12 = nord9;
            color13 = nord15;
            color14 = nord7;
            color15 = nord6;
            normal = {
              black = nord1;
              red = nord11;
              green = nord14;
              yellow = nord13;
              blue = nord9;
              magenta = nord15;
              cyan = nord8;
              white = nord5;
            };
            alt = {
              black = nord3;
              red = nord11;
              green = nord14;
              yellow = nord13;
              blue = nord9;
              magenta = nord15;
              cyan = nord7;
              white = nord6;
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
        #   [ extras.spicetify-nix.packages.x86_64-linux.nord ]
        # else
          # [ ]);
      fonts = {
        fonts = with pkgs; [
          fira-code
          fira-code-symbols
          jetbrains-mono
          siji
          font-awesome-ttf
          roboto-mono
        ];
        fontconfig.defaultFonts = {
          sansSerif = [ "Fira Sans" ];
          monospace = [ "Roboto Mono" ]; # was fira code
        };
      };

      home.configFile = mkIf config.modules.editors.vim.enable {
        "nvim/theme.vim" = { text = "colorscheme stilla"; };
      };
    }
  ]);
}
