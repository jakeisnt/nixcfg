# modules/dev/node.nix --- https://nodejs.org/en/
#
# JS is one of those "when it's good, it's alright, when it's bad, it's a
# disaster" languages.

{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.node;
in {
  options.modules.dev.node = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      nodejs
      yarn

      # :lang javascript
      typescript-language-server
    ];

    # Keep npm's paths valid even when the shell does not define XDG_* vars
    # (for example, in a macOS terminal outside a Nix profile).
    env.NPM_CONFIG_USERCONFIG = "$HOME/.config/npm/config";
    env.NPM_CONFIG_CACHE = "$HOME/.cache/npm";
    env.NPM_CONFIG_PREFIX = "$HOME/.local/npm";
    env.NODE_REPL_HISTORY = "$XDG_CACHE_HOME/node/repl_history";
    env.PATH = [ "$(yarn global bin)" ];

    # Run locally installed bin-script, e.g. n coffee file.coffee
    environment.shellAliases = {
      n = ''PATH="$(npm bin):$PATH"'';
      ya = "yarn";
    };

    home.configFile."npm/config".text = ''
      cache=$XDG_CACHE_HOME/npm
      prefix=$XDG_DATA_HOME/npm
    '';
  };
}
