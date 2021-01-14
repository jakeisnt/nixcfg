{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.matrix;
in {
  options.modules.services.matrix = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services = {
      matrix-synapse = {
      #   enable = true;
      #   server_name = "isnt.online";
      #   registration_shared_secret = "secret";
      #   database_type = "psycopg2";
      #   databse_args = {
      #     database = "matrix-synapse";
      #   };
      #
      #   extraConfig = ''
      #     max_upload_size: "50M"
      #     '';
      };
      # postgresql.enable = true;
    };
  };
}
