{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.gnupg;
in {
  options.modules.shell.gnupg = with types; {
    enable = mkBoolOpt false;
    cacheTTL = mkOpt int 3600; # 1hr
  };

  config = mkIf cfg.enable {
    environment.variables.GNUPGHOME = "$XDG_CONFIG_HOME/gnupg";

    programs.gnupg.agent.enable = true;
    user.packages = with pkgs; [ tomb pinentry.curses pinentry-gnome ];

    # HACK Without this config file you get "No pinentry program" on 20.03.
    #      programs.gnupg.agent.pinentryFlavor doesn't appear to work, and this
    #      is cleaner than overriding the systemd unit.
    # pinentry-program ${pkgs.pinentry.curses}/bin/pinentry
    home.configFile."gnupg/gpg-agent.conf" = {
      text = ''
        allow-emacs-pinentry
        allow-loopback-pinentry
        default-cache-ttl ${toString cfg.cacheTTL}
      '';
    };
  };
}

