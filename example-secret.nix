
{ pkgs, lib, config, modulesPath, inputs, ... }:

let
  agentName = "${config.networking.hostName}";
in {
  config = {
    sops.secrets."buildkite-token" = {
      owner = "buildkite-agent-${agentName}";
      group = "buildkite-agent-${agentName}";
    };

    services.buildkite-agents."${agentName}" = {
      enable = true;
      tokenPath = config.sops.secrets."buildkite-token".path;
    };
  };
}


{ pkgs, config, inputs, ... }:

let
  registrationName = "cole-mickens";
  registrationRepo = "https://github.com/${registrationName}";
in {
  config = {
    sops.secrets."gha-token-${config.networking.hostName}" = {
      #owner = "github-runner";
      #group = "github-runner";
      path = "/var/lib/github-runner/${registrationName}/.token";
    };

    services.github-runner = {
      enable = true;

      url = "https://github.com/${registrationName}";
      name = config.networking.hostName;
      replace = true;
      tokenFile = config.sops.secrets."gha-token-${config.networking.hostName}".path;

      # TODO: seems reasonable to maybe provide vault? (or just use nix for it?)
      # extraPackages = with pkgs; [ vault ];
    };
  };
}
