# Theme modules are a special beast. They're the only modules that are deeply
# intertwined with others, and are solely responsible for aesthetics. Disabling
# a theme module should never leave a system non-functional.

{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.theme;
in {
  options.modules.theme = with types; {
    active = mkOption {
      type = nullOr str;
      default = "stilla";
      apply = v:
        let theme = builtins.getEnv "THEME";
        in if theme != "" then theme else v;
      description = ''
        Name of the theme to enable. Can be overridden by the THEME environment
        variable. Themes can also be hot-swapped with 'hey theme $THEME'.
      '';
    };

    wallpaper = mkOpt (either path null) null;

    color = {
      foreground = mkOpt str "";
      background = mkOpt str "";
      fgAlt = mkOpt str "";
      bgAlt = mkOpt str "";
      bgAlt2 = mkOpt str "";
      fadeColor = mkOpt str "";
      color0 = mkOpt str "";
      color1 = mkOpt str "";
      color2 = mkOpt str "";
      color3 = mkOpt str "";
      color4 = mkOpt str "";
      color5 = mkOpt str "";
      color6 = mkOpt str "";
      color7 = mkOpt str "";
      color8 = mkOpt str "";
      color9 = mkOpt str "";
      color10 = mkOpt str "";
      color11 = mkOpt str "";
      color12 = mkOpt str "";
      color13 = mkOpt str "";
      color14 = mkOpt str "";
      color15 = mkOpt str "";

      # alternative colors for schemes not using the 0-15 color convention
      normal = {
        black = mkOpt str "";
        red = mkOpt str "";
        green = mkOpt str "";
        yellow = mkOpt str "";
        blue = mkOpt str "";
        magenta = mkOpt str "";
        cyan = mkOpt str "";
        white = mkOpt str "";
      };
      alt = {
        black = mkOpt str "";
        red = mkOpt str "";
        green = mkOpt str "";
        yellow = mkOpt str "";
        blue = mkOpt str "";
        magenta = mkOpt str "";
        cyan = mkOpt str "";
        white = mkOpt str "";
      };
      dim = {
        black = mkOpt str "";
        red = mkOpt str "";
        green = mkOpt str "";
        yellow = mkOpt str "";
        blue = mkOpt str "";
        magenta = mkOpt str "";
        cyan = mkOpt str "";
        white = mkOpt str "";
      };
      # used for mako
      urgent = mkOpt str "";
    };

    gtk = {
      theme = mkOpt str "";
      iconTheme = mkOpt str "";
      cursorTheme = mkOpt str "";
    };

    # Extra content for the Firefox userContent.css file
    extraUserContent = mkOpt str "";
    # Extra content for the .Xresources file
    extraXResources = mkOpt str "";

    onReload = mkOpt (attrsOf lines) { };
  };

  config = mkIf (cfg.active != null) (mkMerge [{
    environment.variables.GTK_THEME = cfg.gtk.theme;

    modules.shell.fish.rcInit = with cfg.color; ''
      # if in TTY, configure TTY color scheme
      if [ $TERM = "linux" ]
        set -U fish_color_normal normal
        set -U fish_color_command blue
        set -U fish_color_quote bryellow
        set -U fish_color_redirection brblue
        set -U fish_color_end green
        set -U fish_color_error red
        set -U fish_color_comment brred
        set -U fish_color_match --background=brblue
        set -U fish_color_param blue
        set -U fish_color_selection white --bold --background=brblack
        set -U fish_color_search_match bryellow --background=brblack
        set -U fish_color_history_current --bold
        set -U fish_color_operator brblue
        set -U fish_color_escape black
        set -U fish_color_cwd green
        set -U fish_color_cwd_root red
        set -U fish_color_valid_path --underline
        set -U fish_color_autosuggestion brblack
        set -U fish_color_user brgreen
        set -U fish_color_host normal
        set -U fish_color_cancel -r
        set -U fish_pager_color_completion normal
        set -U fish_pager_color_description yellow
        set -U fish_pager_color_prefix normal --bold --underline
        set -U fish_pager_color_progress brwhite --background=cyan
        clear
      end
    '';

    modules.browsers.firefox.userContent = ''
      @import url("userChrome.css");

      /* Hide scrollbar */
      :root{
          scrollbar-width: none !important;
      }

      /* Removes white loading page */
      @-moz-document url(about:blank), url(about:newtab), url(about:home) {
          html:not(#ublock0-epicker), html:not(#ublock0-epicker) body, #newtab-customize-overlay {
              background: #${cfg.color.background} !important;
          }
      }

      @-moz-document url(about:privatebrowsing) {
          :root{
              scrollbar-width: none !important;
          }
      }
      ${cfg.extraUserContent}
    '';
    home.configFile = {
      # GTK
      "gtk-3.0/settings.ini".text = ''
        [Settings]
        ${optionalString (cfg.gtk.theme != "")
        "gtk-theme-name=${cfg.gtk.theme}"}
        ${optionalString (cfg.gtk.iconTheme != "")
        "gtk-icon-theme-name=${cfg.gtk.iconTheme}"}
        ${optionalString (cfg.gtk.cursorTheme != "")
        "gtk-cursor-theme-name=${cfg.gtk.cursorTheme}"}
        gtk-fallback-icon-theme=gnome
        gtk-application-prefer-dark-theme=true
        gtk-xft-hinting=1
        gtk-xft-hintstyle=hintfull
        gtk-xft-rgba=none
      '';
      # GTK2 global theme (widget and icon theme)
      "gtk-2.0/gtkrc".text = ''
        ${optionalString (cfg.gtk.theme != "")
        ''gtk-theme-name="${cfg.gtk.theme}"''}
        ${optionalString (cfg.gtk.iconTheme != "")
        ''gtk-icon-theme-name="${cfg.gtk.iconTheme}"''}
        gtk-font-name="Sans 10"
      '';
      # QT4/5 global theme
      "Trolltech.conf".text = ''
        [Qt]
        ${optionalString (cfg.gtk.theme != "") "style=${cfg.gtk.theme}"}
      '';
      # Xresources theme
      ".Xresources".text = with cfg.color; ''
        Xft.dpi: 96
        Xft.autohint: 0
        Xft.lcdfilter: lcddefault
        Xft.hintstyle: hintfull
        Xft.hinting: 1
        Xft.antialias: 1
        Xft.rgba: rgb

        !! general
        scratch.font: monospace:pixelsize=32

        *.foreground:   #${foreground}
        *.background:   #${background}
        *.cursorColor:  #${foreground}
        *fading: 35
        *fadeColor: #${fadeColor}

        *.color0: #${color0}
        *.color1: #${color1}
        *.color2: #${color2}
        *.color3: #${color3}
        *.color4: #${color4}
        *.color5: #${color5}
        *.color6: #${color6}
        *.color7: #${color7}
        *.color8: #${color8}
        *.color9: #${color9}
        *.color10: #${color10}
        *.color11: #${color11}
        *.color12: #${color12}
        *.color13: #${color13}
        *.color14: #${color14}
        *.color15: #${color15}
        ${cfg.extraXResources}
      '';
    };
  }]);
}
