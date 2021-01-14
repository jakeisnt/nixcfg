{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.gitea;
    domain = config.networking.domain;
in {
  options.modules.services.gitea = { 
    enable = mkBoolOpt false;
    registration = mkBoolOpt false; 
    mail = mkBoolOpt false;
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
        disableRegistration = cfg.registration;
        httpPort = 3000;

        # We're assuming SSL-only connectivity
        cookieSecure = true;

        log.level = "Error";
        settings.server.DISABLE_ROUTER_LOG = true;
        settings.mailer = mkIf cfg.mail {
          ENABLED = true;
          FROM = "admin@${domain}";
          MAILER_TYPE = "smtp";
          HOST = "localhost:587";
          IS_TLS_ENABLED = true;
          USER = "admin@${domain}";
          PASSWD = secrets.email.hashedPassword;
        };
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
