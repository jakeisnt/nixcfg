{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.i3;
in {
  options.modules.desktop.i3 = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    modules.theme.onReload.i3 = ''
      ${pkgs.bspwm}/bin/bspc wm -r
      source $XDG_CONFIG_HOME/bspwm/bspwmrc
    '';

    environment.systemPackages = with pkgs; [
      lightdm
      dunst
      libnotify
      (polybar.override {
        pulseSupport = true;
        nlSupport = true;
        i3GapsSupport = true;
      })
    ];

    services = {
      picom.enable = true;
      redshift.enable = true;
      xserver = {
        enable = true;
        displayManager = {
          defaultSession = "none+i3";
          lightdm.enable = true;
          lightdm.greeters.mini.enable = true;
        };
        windowManager.i3 = {
          enable = true;
          package = pkgs.i3-gaps;
          # config = rec {
          #   modifier = "Mod4";
          #   focus.followMouse = false;
          #   window.titlebar = false;
          #   gaps = {
          #     smartGaps = true;
          #     outer = 0;
          #     inner = 15;
          #     top = 0;
          #   };

          #   startup = [{
          #     command = "systemctl --user restart polybar";
          #     always = true;
          #     notification = false;
          #   }];

          #   keybindings = { "key combo" = "bound cmd"; };
          # };
        };
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

    # link recursively so other modules can link files in their folders
    home.configFile = {
      "i3" = {
        source = "${configDir}/i3";
        recursive = true;
      };
    };
  };
}
