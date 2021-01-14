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
    yubikey = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    modules.services.acme.enable = true;
    modules.services.nginx.enable = true;

    services = {
      bitwarden_rs = {
        enable = true;
        config = mkMerge [
          {
            domain = "https://bitwarden.${domain}";
            signupsAllowed = cfg.registration;
            webVaultFolder =
              "${pkgs.bitwarden_rs-vault}/share/bitwarden_rs/vault";
            webVaultEnabled = true;
            logFile = "/var/log/bitwarden";
            websocketEnabled = true;
            websocketAddress = "0.0.0.0";
            websocketPort = 3012;
            signupsVerify = true;
            adminToken = secrets.bitwarden.adminToken;
            rocketPort = 8812;
          }
          (mkIf cfg.yubikey {
            yubicoClientId =
              (import /etc/nixos/secret/bitwarden.nix).YUBICO_CLIENT_ID;
            yubicoSecretKey =
              (import /etc/nixos/secret/bitwarden.nix).YUBICO_SECRET_KEY;
            yubicoServer = "https://api.yubico.com/wsapi/2.0/verify";
          })
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
        virtualHosts = {
          "bitwarden.${domain}" = {
            forceSSL = true;
            enableACME = true;
            locations."/" = {
              proxyPass =
                "http://localhost:8812"; # changed the default rocket port due to some conflict
              proxyWebsockets = true;
            };
            locations."/notifications/hub" = {
              proxyPass = "http://localhost:3012";
              proxyWebsockets = true;
            };
            locations."/notifications/hub/negotiate" = {
              proxyPass = "http://localhost:8812";
              proxyWebsockets = true;
            };
          };
        };
      };
    };
    user.packages = with pkgs; [ bitwarden_rs-vault ];
  };
}
