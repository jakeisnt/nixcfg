{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.exwm;
  colors = config.modules.theme.color;
in {
  options.modules.desktop.exwm = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    environment.pathsToLink = [ "/libexec" ];
    services = {
      picom.enable = true;
      redshift = { enable = true; };
      xserver = {
        enable = true;
        dpi = 200;
        layout = "us";
        xkbOptions =
          "caps:swapescape"; # TODO: this should be a per-keyboard configuration.
        libinput.enable = true; # enable touchpad
        desktopManager.xterm.enable = true;

        displayManager = {
          startx.enable = true;
          defaultSession = "none+exwm";
        };

        windowManager.exwm = {
          enable = true;
          enableDefaultConfig = true;

          extraPackages = with pkgs; [
            # notifications
            dunst
            libnotify

            # xorg things, may not be necessary?
            # xorg.xhost
            xorg.xf86inputevdev
            xorg.xf86videointel
            xorg.xf86inputsynaptics
            xorg.xorgserver
          ];
        }
        # windowManager.session = lib.singleton {
        #   name = "exwm";
        #   start = ''
        #     xhost +SI:localuser:$USER
        #     exec emacs
        #   '';
        # }
        ;

        # todo: device-specific
        monitorSection = ''
          DisplaySize 508 285
        '';
      };
    };

    systemd.user.services."dunst" = {
      enable = true;
      description = "";
      wantedBy = [ "default.target" ];
      serviceConfig.Restart = "always";
      serviceConfig.RestartSec = 2;
      serviceConfig.ExecStart = "${pkgs.dunst}/bin/dunst";
    };

    env.XDG_CURRENT_DESKTOP = "emacs";
    home.configFile = { };
  };
}
