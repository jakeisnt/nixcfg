{ config, options, pkgs, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.fish;
in {
  options.modules.shell.fish = with types; {
    enable = mkBoolOpt false;

    aliases = mkOpt (attrsOf (either str path)) { };

    rcInit = mkOpt' lines "" ''
      Zsh lines to be written to $XDG_CONFIG_HOME/zsh/extra.zshrc and sourced by
      $XDG_CONFIG_HOME/zsh/.zshrc
    '';
    loginInit = mkOpt' lines "" ''
      Zsh lines to be written to $XDG_CONFIG_HOME/zsh/extra.zshenv and sourced
      by $XDG_CONFIG_HOME/zsh/.zshenv
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
       fish_add_path /etc/nixos/bin
       ${starship}/bin/starship init fish | source
       ${zoxide}/bin/zoxide init fish | source
       ${cfg.rcInit}
     '';

      loginShellInit = cfg.loginInit;

      shellAliases = with pkgs; {
        # use the souped-up rust stuff!
        "ls" = "${exa}/bin/exa";
        "cat" = "${bat}/bin/bat";
        "find" = "${fd}/bin/fd";
        "ps" = "${procs}/bin/procs";
        "grep" = "${ripgrep}/bin/rg";
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
  };
}
