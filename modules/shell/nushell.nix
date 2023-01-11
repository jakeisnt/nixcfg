{ config, options, pkgs, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.nushell;
    loginInit = config.modules.shell.loginInit;
in {
  options.modules.shell.loginInit = with types; mkOpt' lines "" ''
    Shell commands to be run before the shell starts up. Assume POSIX-compatibility here.
   '';

  options.modules.shell.nushell = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    users.defaultUserShell = pkgs.bash;

    programs.bash = {
      interactiveShellInit = with pkgs; ''
        ${pkgs.nushell}/bin/nu
      '';
      loginShellInit = loginInit;
    };

    user.packages = with pkgs; [
      nushell
      starship
      bat
    ];

    home.configFile = {
      "starship.toml".text = (concatMapStringsSep "\n" readFile [ "${configDir}/starship/starship.toml" ]);
      "nushell/config.nu".text = (concatMapStringsSep "\n" readFile [ "${configDir}/nushell/config.nu" ]);
      "nushell/env.nu".text = (concatMapStringsSep "\n" readFile [ "${configDir}/nushell/env.nu" ]);
    };
  };
}
