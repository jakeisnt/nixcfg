{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.messengers.email;
in {
  options.modules.messengers.email = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      [ ] ++ (if config.services.xserver.enable then
        [
          sylpheed
          # alternatives: thunderbird (maybe bloated, but may support office365), claws (different ui)
          # TODO: use some daemon i can fetch email through independent of the client
        ]
      else
        [ neomutt ]);
    # todo: add some more config to set up email automatically depending on account!
    # alternatives: interfaces within emacs like mu4e
  };
}
