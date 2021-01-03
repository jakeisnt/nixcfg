{ options, config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.ssh;
in {
  options.modules.services.ssh = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services.openssh = {
      enable = true;
      challengeResponseAuthentication = false;
      passwordAuthentication = false;
    };

    user.openssh.authorizedKeys.keys = [''
      ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGlx01MslXiUGleFZrb50+8WG73VfX5HUnrN0Xwgb4Xj jakeisnt@github.com
    ''];
  };
}
