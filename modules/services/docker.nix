{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.docker;
in {
  options.modules.services.docker = {
    enable = mkBoolOpt false;
    podman = mkBoolOpt false;
    kubernetes = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; ([
      docker-client
      docker-compose
    ] ++ (if cfg.kubernetes then [ kubectl k3s ] else []));

    env.DOCKER_CONFIG = "$XDG_CONFIG_HOME/docker";
    env.MACHINE_STORAGE_PATH = "$XDG_DATA_HOME/docker/machine";

    user.extraGroups = [ "docker" ];

    environment.shellAliases = {
        "dk" = "docker";
        "dkc" = "docker-compose";
        "dkl" = "dk logs";
        "dkcl" = "dkc logs";
        "dkclr" = "dk stop (docker ps -a -q) && dk rm (docker ps -a -q)";
    };

    # a nixos kubernetes service running in userspace (I hope
    # TODO: This doesn't work.
    # services.kubernetes = mkIf cfg.kubernetes {
    #   roles = ["master" "node"];
    #   masterAddress = "localhost";
    #   apiserver.enable = true;
    #   controllerManager.enable = true;
    #   scheduler.enable = true;
    #   addonManager.enable = true;
    #   proxy.enable = true;
    #   flannel.enable = true;
    # };

    virtualisation = {
      podman = {
        enable = cfg.podman;
        dockerCompat = true;
        dockerSocket.enable = true;
      };
      docker = {
        enable = !cfg.podman;
        # autoPrune.enable = true;
        enableOnBoot = true;
        # listenOptions = [];
      };
    };
    users.users.jake.extraGroups = [ "podman" ];
  };
}
