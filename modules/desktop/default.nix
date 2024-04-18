{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop;
in {
  config = mkIf config.modules.desktop.sway.enable {
    user.packages = with pkgs; [ feh ];

    fonts = {
      fontDir.enable = true;
      enableGhostscriptFonts = true;
      packages = with pkgs; [ nerdfonts noto-fonts ];
    };

    ## Apps/Services
    # Try really hard to get QT to respect my GTK theme.
    env.GTK_DATA_PREFIX = [ "${config.system.path}" ];
    env.QT_QPA_PLATFORMTHEME = "gtk2";
    qt = {
      style = "gtk2";
      platformTheme = "gtk2";
    };

    services.xserver.displayManager.sessionCommands = ''
      # GTK2_RC_FILES must be available to the display manager.
      export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
    '';

    # set user dirs
    home.configFile = {
      "user-dirs.dirs".source = "${configDir}/xdg/user-dirs.dirs";
    };
  };
}
