{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.services.bitwarden;
  domain = config.networking.domain;
in {
  options.modules.services.bitwarden = {
    enable = mkBoolOpt false;
    registration = mkBoolOpt false;
    mail = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    modules.services.acme.enable = true;
    modules.services.nginx.enable = true;

    services = {
      vaultwarden = {
        enable = true;
        config = mkMerge [
          {
            domain = "https://bitwarden.${domain}";
            signupsAllowed = cfg.registration;
            invitationsAllowed = cfg.registration;
            webVaultFolder =
              "${pkgs.vaultwardens-vault}/share/bitwarden_rs/vault";
            webVaultEnabled = true;
            logFile = "/var/log/bitwarden";
            websocketEnabled = true;
            websocketAddress = "0.0.0.0";
            websocketPort = 3012;
            signupsVerify = true;
            adminToken = secrets.bitwarden.adminToken;
            rocketPort = 8812;
          }
          (mkIf cfg.mail {
            smtpHost = "localhost";
            smtpFrom = "admin@${domain}";
            smtpFromName = "Admin";
            smtpPort = 587;
            smtpSsl = true;
            smtpUsername = "admin@${domain}";
            smtpPassword = secrets.email.hashedPassword;
            smtpTimeout = 15;
          })
        ];
      };

      nginx = {
        virtualHosts."bitwarden.${domain}" = {
            forceSSL = true;
            enableACME = true;
            root = "/srv/www/bitwarden.${domain}";
            locations = {
              "/" = {
                proxyPass =
                  "http://localhost:8812"; # changed the default rocket port due to some conflict
                proxyWebsockets = true;
              };
              "/notifications/hub" = {
                proxyPass = "http://localhost:3012";
                proxyWebsockets = true;
              };
              "/notifications/hub/negotiate" = {
                proxyPass = "http://localhost:8812";
                proxyWebsockets = true;
              };
            };
        };
      };
    };

    user.extraGroups = [ "vaultwarden" ];
    user.packages = with pkgs; [ vaultwarden-vault ];
  };
}
