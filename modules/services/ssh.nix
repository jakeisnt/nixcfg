{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.services.ssh;
  add = "/run/current-system/sw/bin/ssh-add";
  keygen = "/run/current-system/sw/bin/ssh-keygen";
  ssh-agent = "/run/current-system/sw/bin/ssh-agent";
in {
  options.modules.services.ssh = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services.openssh = {
      enable = true;
      challengeResponseAuthentication = false;
      passwordAuthentication = false;
    };

    user.packages = with pkgs;
      [
        (writeScriptBin "ssh-key" ''
          #!${stdenv.shell}
          # Create SSH key
          ${keygen} -t rsa -b 4096 -C "youremail@domain.com"
          eval $(${ssh-agent} -s)
          ${add} $HOME/.ssh/id_rsa
        '')
      ];
  };
}
