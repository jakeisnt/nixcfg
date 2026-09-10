{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.postgresql;
in {
  options.modules.dev.postgresql = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # Keep the database local by default; applications on this machine, such
    # as Improvin, can connect through PostgreSQL's Unix socket or localhost.
    services.postgresql = {
      enable = true;
      # The legacy `pkgs.postgresql` alias in this configuration resolves to
      # PostgreSQL 11, which has been removed from the current nixpkgs.
      package = pkgs.postgresql_18;
    };

    # `services.postgresql` provides the daemon, while this makes `psql` and
    # the other client/admin tools available in development shells.
    user.packages = [ pkgs.postgresql_18 ];
  };
}
