{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.gitea;
in {
  options.modules.services.gitea = { enable = mkBoolOpt false; };

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

        user = "git";
        database = {
          user = "git";
          type = "postgres";
          createDatabase = true;
        };

        ssh = {
          clonePort = 2222;
        };

        disableRegistration = true;

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

      postgresql = {
        enable = true;
        initialScript = pkgs.writeText "gitea-init.sql" ''
            CREATE USER "git";
          '';
      };
    };

    user.extraGroups = [ "gitea" ];
  };
}
