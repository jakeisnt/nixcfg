{ config, options, pkgs, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.fish;
    loginInit = config.modules.shell.loginInit;
in {
  options.modules.shell.fish = with types; {
    enable = mkBoolOpt false;

    aliases = mkOpt (attrsOf (either str path)) { };

    rcInit = mkOpt' lines "" ''
        Fish commands to be run when fish starts up.
    '';
    loginInit = mkOpt' lines "" ''
      Other fish commands to be run before fish starts up. It's worth noting that these run before 'rcInit' commands.
    '';

    rcFiles = mkOpt (listOf (either str path)) [ ];
    envFiles = mkOpt (listOf (either str path)) [ ];
  };

  config = mkIf cfg.enable {
    users.defaultUserShell = pkgs.fish;

    programs.fish = {
      enable = true;
      promptInit = with pkgs; ''
       set fish_greeting
       function fish_mode_prompt; end
       function fish_prompt; end

       function postexec_test --on-event fish_postexec
          echo
       end

       fish_add_path /etc/nixos/bin
       ${starship}/bin/starship init fish | source
       ${zoxide}/bin/zoxide init fish | source
       ${cfg.loginInit}
       ${cfg.rcInit}
     '';

      loginShellInit = loginInit;

      shellAliases = with pkgs; {
        # use the souped-up rust stuff!
        "ls" = "${exa}/bin/exa";
        "cat" = "${bat}/bin/bat";
        "find" = "${fd}/bin/fd";
        "ps" = "${procs}/bin/procs";
        "grep" = "${ripgrep}/bin/rg";

        # other sane shell reconfigurations
        "q" = "exit";
        "cp" = "cp -i";
        "mv" = "mv -i";
        "rm" = "rm -i";
        "mkdir" = "mkdir -p";
        "sc" = "systemctl";
        "ssc" = "sudo systemctl";

        # cool idea: remind me later!
        # r() {
        #   local time=$1; shift
        #   sched "$time" "notify-send --urgency=critical 'Reminder' '$@'; ding";
        # }; compdef r=sched
      };
    };

    user.packages = with pkgs; [
      starship
      bat
      exa
      fd
      procs
      ripgrep
      tokei
      tealdeer # tldr
      fzf
      zoxide
    ];

    home.configFile = {
      "starship.toml".text = (concatMapStringsSep "\n" readFile [ "${configDir}/starship/starship.toml" ]);
    };
  };
}
