{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.matrix;
    domain = config.networking.domain;
in {
  options.modules.services.matrix = { 
    enable = mkBoolOpt false;
    registration = mkBoolOpt false;
    element = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    modules.services.acme.enable = true;
    modules.services.nginx.enable = true;

    networking.firewall.allowedTCPPorts = [
      8448 # Matrix federation
    ];

    services = mkMerge [
      (mkIf cfg.element {
        nginx.virtualHosts."element.${domain}" = {
          enableACME = true;
          forceSSL = true;

          root = pkgs.element-web.override {
            conf = {
              default_server_config."m.homeserver" = {
                "server_name" = domain;
                "base_url" = "https://matrix.${domain}";
              };

              jitsi.preferredDomain = mkIf config.modules.services.jitsi.enable "jitsi.${domain}";
            };
          };
        };
      })
      {
        matrix-synapse = {
          enable = true;
          server_name = domain;
          public_baseurl = "https://matrix.${domain}";
          enable_registration = cfg.registration;
          registration_shared_secret = secrets.matrix.password;

          database_type = "psycopg2";
          database_args = { database = "matrix-synapse"; };

          listeners = [
            { 
              port = 8008;
              bind_address = "::1";
              type = "http";
              tls = false;
              x_forwarded = true;
              resources = [
                {
                  names = [ "client" "federation" ];
                  compress = false;
                }
              ];
            }
          ];

          extraConfig = ''
            max_upload_size: "100M"
          '';
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

        nginx = {
          virtualHosts = {
            domain = {
              locations."= /.well-known/matrix/server".extraConfig =
                let
                  # use 443 instead of the default 8448 port
                  server = { "m.server" = "matrix.${domain}:443"; };
                in ''
                  add_header Content-Type application/json;
                  return 200 '${builtins.toJSON server}';
                '';
              locations."= /.well-known/matrix/client".extraConfig =
                let
                  client = {
                    "m.homeserver" =  { "base_url" = "https://matrix.${domain}"; };
                    "m.identity_server" =  { "base_url" = "https://vector.im"; };
                  };
                in ''
                  add_header Content-Type application/json;
                  add_header Access-Control-Allow-Origin *;
                  return 200 '${builtins.toJSON client}';
                '';
            };

            "matrix.${domain}" = {
              enableACME = true;
              forceSSL = true;

              locations."/".extraConfig = ''
                return 404;
              '';

              locations."/_matrix" = {
                proxyPass = "http://[::1]:8008";
              };
            };
          };
        };
      }
    ];
  };
}
