# modules/themes/alucard/default.nix --- a regal dracula-inspired theme

{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.theme;
in {
  config = mkIf (cfg.active == "nordic") (mkMerge [
    # Desktop-agnostic configuration
    {
      modules = {
        theme = {
          gtk = {
            theme = "Nordic";
            iconTheme = "Paper";
            cursorTheme = "Paper";
          };
        };

        shell.zsh.rcFiles = [ ./config/zsh/prompt.zsh ];
        shell.tmux.rcFiles = [ ./config/tmux.conf ];
        desktop.browsers = {
          firefox.userChrome = concatMapStringsSep "\n" readFile
            [ ./config/firefox/userChrome.css ];
          firefox.userContent = concatMapStringsSep "\n" readFile
            [ ./config/firefox/userContent.css ];
        };
      };
    }

    # Desktop theming
    {
      user.packages = with pkgs; [ nordic ];
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
          {
            # Sourced from sessionCommands in modules/themes/default.nix
            "xtheme/90-theme".source = ./config/Xresources;
          }
          (mkIf desktop.media.graphics.vector.enable {
            "inkscape/templates/default.svg".source =
              ./config/inkscape/default-template.svg;
          })
        ];
    }
  ]);
}
