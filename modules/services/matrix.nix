{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.matrix;
in {
  options.modules.services.matrix = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services = {
      matrix-synapse = {
        enable = true;
        server_name = secrets.matrix.server_name;
        enable_registration = true;
        registration_shared_secret = secrets.matrix.password;

        public_baseurl = "https://matrix.${secrets.matrix.server_name}";
        tls_certificate_path = "/var/lib/acme/matrix.isnt.online/fullchain.pem";
        tls_private_key_path = "/var/lib/acme/matrix.isnt.online/key.pem";

        database_type = "psycopg2";
        database_args = { database = "matrix-synapse"; };

        listeners = [
          { # federation
            bind_address = "";
            port = 8448;
            resources = [
              {
                compress = true;
                names = [ "client" "webclient" ];
              }
              {
                compress = false;
                names = [ "federation" ];
              }
            ];

            tls = true;
            type = "http";
            x_forwarded = false;
          }
          { # client
            bind_address = "127.0.0.1";
            port = 8008;
            resources = [{
              compress = true;
              names = [ "client" "webclient" ];
            }];
            tls = false;
            type = "http";
            x_forwarded = true;
          }
        ];

        extraConfig = ''
          max_upload_size: "100M"
        '';
      };

      nginx = {
        enable = true;
        recommendedTlsSettings = true;
        recommendedOptimisation = true;
        recommendedGzipSettings = true;
        recommendedProxySettings = true;

        virtualHosts = {
          "matrix.${secrets.matrix.server_name}" = {
            forceSSL = true;
            enableACME = true;
            locations."/" = { proxyPass = "http://127.0.0.1:8008"; };
          };
        };
      };

      postgresql = {
        enable = true;
        initialScript = pkgs.writeText "synapse-init.sql" ''
          CREATE USER "matrix-synapse";

          CREATE DATABASE "matrix-synapse"
              ENCODING 'UTF8'
              LC_COLLATE='C'
              LC_CTYPE='C'
              template=template0
              OWNER "matrix-synapse";
        '';
      };
    };

    security.acme.certs = {
      "matrix.${secrets.matrix.server_name}" = {
        group = "matrix-synapse";
        postRun =
          "systemctl reload nginx.service; systemctl restart matrix-synapse.service";
      };
    };
  };
}
