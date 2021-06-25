{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.wayfire;
  colors = config.modules.theme.color;
  startwf = (pkgs.writeTextFile {
    name = "startwf";
    destination = "/bin/startwf";
    executable = true;
    text = ''
      #! ${pkgs.bash}/bin/bash

      # first import environment variables from the login manager
      systemctl --user import-environment
      # then start the service
      exec systemctl --user start wf.service
    '';
  });
in {
  options.modules.desktop.wayfire = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    modules.wayland.enable = true;
    modules.desktop.apps.wofi.enable = true;
    modules.wayland.mako.enable = true;
    modules.wayland.swaylock.enable = true;
    modules.wayland.waybar.enable = true;
    modules.wayland.kanshi.enable = true;

    user.packages = with pkgs; [ wayfire ];

    env.XDG_CURRENT_DESKTOP = "wayfire";

    systemd.user.targets.wayfire-session = {
      enable = true;
      description = "Wayfire compositor session";
      documentation = [ "man:systemd.special(7)" ];

      bindsTo = [ "graphical-session.target" ];
      wants = [ "graphical-session-pre.target" ];
      after = [ "graphical-session-pre.target" ];
      requiredBy =
        [ "graphical-session.target" "graphical-session-pre.target" ];
    };

    systemd.user.services.wf = {
      enable = true;
      description = "Wayfire - Wayland window manager";
      bindsTo = [ "graphical-session.target" ];
      wants = [ "graphical-session-pre.target" ];
      after = [ "graphical-session-pre.target" ];
      # We explicitly unset PATH here, as we want it to be set by
      # systemctl --user import-environment in startsway
      environment.PATH = lib.mkForce null;
      serviceConfig = {
        Type = "simple";
        ExecStart =
          "${pkgs.dbus}/bin/dbus-run-session ${pkgs.wayfire}/bin/wayfire";
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
    };

    modules.shell.zsh.rcInit = ''
      if [ -z $DISPLAY ] && [ "$(tty)" == "/dev/tty1" ]; then
        startwf
      fi
    '';

    home.configFile = {
      "wayfire/config" = { source = "${configDir}/wayfire/config"; };
    };
  };
}
