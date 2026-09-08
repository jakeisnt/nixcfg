{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.acme;
in {
  options.modules.services.acme = {
    enable = mkBoolOpt false;
    # Public account metadata; credentials must not be placed here.
    email = mkOpt (types.nullOr types.str) null;
  };

  config = mkIf cfg.enable {
    security.acme = {
      acceptTerms = true;
    } // optionalAttrs (cfg.email != null) {
      defaults.email = cfg.email;
      email = cfg.email;
    };
  };
}
