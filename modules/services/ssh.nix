{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.services.ssh;
  add = "/run/current-system/sw/bin/ssh-add";
  keygen = "/run/current-system/sw/bin/ssh-keygen";
  ssh-agent = "/run/current-system/sw/bin/ssh-agent";
  xpsKey =
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCtB3BbMTx18grI1xxJ+mvjO8jzYoUIWh6l230DiXmQ7w7+3ZJpPEh21UphqZvm/k5FwsPU5Tb+vuwofEjPZfvx0ERL1XNvrFg/2rQmGl2ccsupclMz2uxwP47APW5amJzhVRUejTOhv9Xo7kaZGqeh3L5hbyNSfC3Ci2LpZpV1nwuOgqmCm58d68G05YrW/LIM7zPVpWRa7/1Jyplu2AYViCB5z3jt1q1s7UP2oXAh5bH5QQ5vlqdsU9GEpn0HSF0CjQdGx7PDfyeocDoI0Dy0coUCWsMDrPqURnZWzQL0eARKkaYdLHcJ2N8I3dwU1z8I/8VJ+jxm9jmOxhuwlIluXdz6VUbh+eNOmEczVwjAtTdpiyBG2ku0labQ4TMN0G0GdV3USxC4sEfBYfzF1m2NERRAm9ySkjxbLqQ5b/85mvLUtuJ4q5hMla6gGDRRKUAWGrDlVe8G3NACARqqDIQOE36Je/u/f3HjOFtczEf5pfAUW4Ky7tyOJYzaxErPK/zc1g2qRdVtDclgdI1IHuSFywQPeAkz0l0vTqe60+RUZT0kyQXNzvZeEZ9U8ia8kO07HfxGAu3cdW7AJI/5eZv1LXI6Xa0GYN+9BSARz6RKHjNIM9wYAqGrDExgo56UOLrHqiDRRt6ZaqBPXzBdIp6D6HHuh7oggexINXoRvHaMWw== jake@isnt.online";
in {
  options.modules.services.ssh = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services.openssh = {
      enable = true;
      challengeResponseAuthentication = false;
      passwordAuthentication = false;
    };

    # Allow SSHing through firewall
    networking.firewall.allowedTCPPorts = [ 22 ];
    user.openssh.authorizedKeys.keys = [ xpsKey ];

    user.packages = with pkgs;
      [
        (writeScriptBin "ssh-key" ''
          #!${stdenv.shell}
          # Create SSH key
          ${keygen} -t rsa -b 4096 -C "${secrets.username}@${secrets.domain}"
          eval $(${ssh-agent} -s)
          ${add} $HOME/.ssh/id_rsa
        '')
      ];
  };
}
