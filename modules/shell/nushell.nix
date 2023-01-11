{ config, options, pkgs, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.nushell;
in {
  options.modules.shell.nushell = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    users.defaultUserShell = pkgs.nushell;
    user.packages = with pkgs; [
      nushell
      starship
    ];

    home.configFile = {
      "starship.toml".text = (concatMapStringsSep "\n" readFile [ "${configDir}/starship/starship.toml" ]);
      "nushell/config.nu".text = (concatMapStringsSep "\n" readFile [ "${configDir}/nushell/config.nu" ]);
      "nushell/env.nu".text = (concatMapStringsSep "\n" readFile [ "${configDir}/nushell/env.nu" ]);
    };
  };
}
