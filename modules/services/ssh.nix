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
      passwordAuthentication = true; # TODO: configure SSH keys properly and make this false
    };

    # Allow SSHing through firewall
    networking.firewall.allowedTCPPorts = [
      22
    ];

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
