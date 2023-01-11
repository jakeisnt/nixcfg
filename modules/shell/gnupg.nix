{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.gnupg;
in {
  options.modules.shell.gnupg = with types; {
    enable = mkBoolOpt false;
    gui = mkBoolOpt false;
    cacheTTL = mkOpt int 3600; # 1hr
  };

  config = mkIf cfg.enable {
    environment.variables.GNUPGHOME = "$XDG_CONFIG_HOME/gnupg";
    programs.gnupg.agent.enable = true;
    user.packages = with pkgs; [
      tomb
      # all three are necessary for some reason
      pinentry.curses
      pinentry-gnome
      pinentry-emacs
    ];

    # HACK Without this config file you get "No pinentry program" on 20.03.
    #      programs.gnupg.agent.pinentryFlavor doesn't appear to work, and this
    #      is cleaner than overriding the systemd unit.
    # pinentry-program ${pkgs.pinentry.curses}/bin/pinentry
    # TODO: this may not line up with emacs
    programs.gnupg.agent.pinentryFlavor = "emacs";
    home.configFile."gnupg/gpg.conf" = {
      text = ''
      pinentry-mode loopback
     '';
    };
    home.configFile."gnupg/gpg-agent.conf" = {
      text = ''
        allow-emacs-pinentry
        allow-loopback-pinentry
        enable-ssh-support
        pinentry-program ${if config.modules.editors.emacs.enable then pkgs.pinentry-emacs else (if cfg.gui then pkgs.pinentry-gnome else pkgs.pinentry-curses)}/bin/pinentry
        default-cache-ttl ${toString cfg.cacheTTL}
        max-cache-ttl ${toString cfg.cacheTTL}
      '';
    };
  };
}
