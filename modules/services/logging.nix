{ config, options, lib, pkgs, ... }:

# tutorial: https://christine.website/blog/prometheus-grafana-loki-nixos-2020-11-20

with lib;
with lib.my;
let
  cfg = config.modules.services.logging;
  domain = config.networking.domain;
in {
  options.modules.services.logging = {
    enable = mkBoolOpt false;
    email = mkBoolOpt false;
    grafana = mkBoolOpt true;
    prometheus = mkBoolOpt true;
    loki = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    services.grafana = mkIf cfg.grafana {
      enable = true;
      domain = "grafana.${domain}";
      port = 2342;
      addr = "127.0.0.1";

      provision = {
        enable = true;
        dashboards = [{
          name = "Prometheus";
          url = "localhost:9001";
          # access: server (default)
        }];
      };
    };

    # TODO: configure more grafana information to display

    services.prometheus = mkIf cfg.prometheus {
      enable = true;
      port = 9001;
      exporters = {
        node = {
          enable = true;
          enabledCollectors = [ "systemd" ];
          port = 9002;
        };
      };

      scrapeConfigs = [{
        job_name = config.networking.hostName;
        static_configs = [{
          targets = [
            "127.0.0.1:${
              toString config.services.prometheus.exporters.node.port
            }"
          ];
        }];
      }];
    };

    services.loki = {
      enable = true;
      configuration = {
        auth_enabled = false;
        server = { http_listen_port = 3100; };
        ingester = {
          lifecycler = {
            address = "0.0.0.0";
            ring = {
              kvstore = { store = "inmemory"; };
              replication_factor = 1;
            };
            final_sleep = "0s";
          };
          chunk_idle_period =
            "1h"; # Any chunk not receiving new logs in this time will be flushed
          max_chunk_age =
            "1h"; # All chunks will be flushed when they hit this age, default is 1h
          chunk_target_size =
            1048576; # Loki will attempt to build chunks up to 1.5MB, flushing first if chunk_idle_period or max_chunk_age is reached first
          chunk_retain_period =
            "30s"; # Must be greater than index read cache TTL if using an index cache (Default index read cache TTL is 5m)
          max_transfer_retries = 0; # Chunk transfers disabled
        };

        schema_config = {
          configs = {
            from = "2020-10-24";
            store = "boltdb-shipper";
            object_store = "filesystem";
            schema = "v11";
            index = {
              prefix = "index_";
              period = "24h";
            };
          };
        };
        storage_config = {
          boltdb_shipper = {
            active_index_directory = "/var/lib/loki/boltdb-shipper-active";
            cache_location = "/var/lib/loki/boltdb-shipper-cache";
            cache_ttl =
              "24h"; # Can be increased for faster performance over longer query periods, uses more disk space
            shared_store = "filesystem";
          };
          filesystem = { directory = "/var/lib/loki/chunks"; };
        };

        limits_config = {
          reject_old_samples = true;
          reject_old_samples_max_age = "168h";
        };

        chunk_store_config = { max_look_back_period = "0s"; };

        table_manager = {
          retention_deletes_enabled = false;
          retention_period = "0s";
        };
      };
    };

    systemd.services.promtail = {
      description = "Promtail service for Loki";
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        ExecStart = ''
          ${pkgs.grafana-loki}/bin/promtail --config.file ${configDir}/promtail/promtail.yaml
        '';
      };
    };

    home.configFile = {
      "promtail/promtail.yaml".text = ''
        server:
          http_listen_port: 28183
          grpc_listen_port: 0

        positions:
          filename: /tmp/positions.yaml

        clients:
          - url: http://127.0.0.1:3100/loki/api/v1/push

        scrape_configs:
          - job_name: journal
            journal:
              max_age: 12h
              labels:
                job: systemd-journal
                host: chrysalis
            relabel_configs:
              - source_labels: ['__journal__systemd_unit']
                target_label: 'unit'
      '';
    };

    services.nginx.virtualHosts.${config.services.grafana.domain} =
      mkIf cfg.grafana {
        forceSSL = true;
        enableACME = true;
        locations."/" = {
          proxyPass =
            "http://127.0.0.1:${toString config.services.grafana.port}";
          proxyWebsockets = true;
        };
      };
  };
}
