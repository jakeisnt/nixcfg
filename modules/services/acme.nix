{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.acme;
in {
  options.modules.services.acme = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    security.acme = {
      defaults.email = "${secrets.gmail.user}";
      email = "${secrets.gmail.user}";
      acceptTerms = true;
    };
  };
}
