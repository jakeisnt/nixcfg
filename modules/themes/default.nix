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
      default = null;
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
      fadeColor = mkOpt str "";
      inactiveBack = mkOpt str "";
      pHoldBack = mkOpt str "";
      focusedBack = mkOpt str "";
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
      # currently used for alacritty,
      # it may be better to switch to this over numbers
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
      main = {
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
    # TTY Theme
    modules.shell.zsh.rcInit = with cfg.color; ''
      # if in TTY, configure TTY color scheme
      if [ "$TERM" = "linux" ]; then
        echo -en "\e]P0${color0}"
        echo -en "\e]P1${color1}"
        echo -en "\e]P2${color2}"
        echo -en "\e]P3${color3}"
        echo -en "\e]P4${color4}"
        echo -en "\e]P5${color5}"
        echo -en "\e]P6${color6}"
        echo -en "\e]P7${color7}"
        echo -en "\e]P8${color8}"
        echo -en "\e]P9${color9}"
        echo -en "\e]PA${color10}"
        echo -en "\e]PB${color11}"
        echo -en "\e]PC${color12}"
        echo -en "\e]PD${color13}"
        echo -en "\e]PE${color14}"
        echo -en "\e]PF${color15}"
        clear
      fi
    '';

    modules.desktop.browsers.firefox.userContent = ''
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
        gtk-button-images=1
        gtk-menu-images=1
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
