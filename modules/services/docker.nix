{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.docker;
in {
  options.modules.services.docker = {
    enable = mkBoolOpt false;
    podman = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      # docker
      # arion
      docker-client
      docker-compose
    ];

    env.DOCKER_CONFIG = "$XDG_CONFIG_HOME/docker";
    env.MACHINE_STORAGE_PATH = "$XDG_DATA_HOME/docker/machine";

    user.extraGroups = [ "docker" ];
    modules.shell.zsh.rcFiles = [ "${configDir}/docker/aliases.zsh" ];

    virtualisation = {
      podman = {
        enable = cfg.podman;
        dockerCompat = true;
        dockerSocket.enable = true;
        defaultNetwork.dnsname.enable = true;
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
