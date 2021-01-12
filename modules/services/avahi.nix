{ options, config, lib, pkgs, ... }:

# Make systems discoverable to one another over local networks.

with lib;
with lib.my;
let cfg = config.modules.services.avahi;
in {
  options.modules.services.avahi = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services.avahi = {
      enable = true;
      nssmdns = true;
      publish.enable = true;
      publish.addresses = true;
      publish.workstation = true;
    };
  };
}
