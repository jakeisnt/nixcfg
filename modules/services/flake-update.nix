{ config, lib, pkgs, ... }:
let
  cfg = config.modules.services.flake-update;
  user = config.users.users.${config.user.name};
  updater = pkgs.writeShellApplication {
    name = "flake-update";
    runtimeInputs = with pkgs; [ git nix openssh jq util-linux coreutils libnotify ];
    text = builtins.readFile ../../bin/flake-update;
  };
in {
  options.modules.services.flake-update = {
    enable = lib.mkEnableOption "weekly verified flake update branches";
    repository = lib.mkOption {
      type = lib.types.str;
      default = "/etc/nixos";
      description = "Local Git repository used to create update worktrees.";
    };
    baseBranch = lib.mkOption {
      type = lib.types.str;
      default = "main";
      description = "Branch fetched from origin before preparing an update.";
    };
    calendar = lib.mkOption {
      type = lib.types.str;
      default = "Sun *-*-* 09:00:00";
      description = "systemd calendar expression for preparing updates.";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.timers.flake-update = {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = cfg.calendar;
        Persistent = true;
        RandomizedDelaySec = "30m";
      };
    };
    systemd.services.flake-update = {
      description = "Build and publish a flake update for review";
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      environment = {
        HOME = user.home;
        DBUS_SESSION_BUS_ADDRESS = "unix:path=/run/user/${toString user.uid}/bus";
      };
      serviceConfig = {
        Type = "oneshot";
        User = config.user.name;
        StateDirectory = "flake-update";
        StateDirectoryMode = "0700";
        ExecStart = "${lib.getExe updater} ${lib.escapeShellArgs [ cfg.repository cfg.baseBranch ]}";
        TimeoutStartSec = "12h";
        Nice = 10;
        IOSchedulingClass = "idle";
      };
    };
  };
}
