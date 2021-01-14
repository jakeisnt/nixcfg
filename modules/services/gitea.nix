{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.gitea;
    domain = config.networking.domain;
in {
  options.modules.services.gitea = { 
    enable = mkBoolOpt false;
    registration = mkBoolOpt false; 
  };

  config = mkIf cfg.enable {
    modules.services.nginx.enable = true;

    # I prefer git@... ssh addresses over gitea@...
    users.users.git = {
      useDefaultShell = true;
      home = "/var/lib/gitea";
      group = "gitea";
    };

    services = {
      gitea = {
        enable = true;
        domain = "git.${domain}";
        appName = "git.${domain}";
        rootUrl = "git.${domain}";

        user = "git";
        database = {
          user = "git";
          type = "postgres";
          createDatabase = true;
        };

        ssh = {
          enable = true;
          clonePort = 2222;
        };

        dump.enable = true;
        disableRegistration = false;
        httpPort = 3000;

        # We're assuming SSL-only connectivity
        cookieSecure = true;

        log.level = "Error";
        settings.server.DISABLE_ROUTER_LOG = true;
      };

      nginx = {
        virtualHosts = {
          "git.${domain}" = {
            forceSSL = true;
            enableACME = true;
            locations."/" = { proxyPass = "http://127.0.0.1:3000"; };
          };
        };
      };
    };

    user.extraGroups = [ "gitea" ];
  };
}
