{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.git;
in {
  options.modules.shell.git = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      git
      gitflow
      gitAndTools.gh
      gitAndTools.git-open
      gitAndTools.diff-so-fancy
      gitAndTools.git-absorb
      (mkIf config.modules.shell.gnupg.enable gitAndTools.git-crypt)
    ];

    environment.shellAliases = with pkgs; {
      "ga" = "${git}/bin/git add";
      "gap" = "${git}/bin/git add --patch";
      "gb" = "${git}/bin/git branch -av";
      "gop" = "${git}/bin/git open";
      "gbl"="${git}/bin/git blame";
      "gc"="${git}/bin/git commit";
      "gcm"="${git}/bin/git commit -m";
      "gca"="${git}/bin/git commit --amend";
      "gcf"="${git}/bin/git commit --fixup";
      "gcl"="${git}/bin/git clone";
      "gco"="${git}/bin/git checkout";
      "gcoo"="${git}/bin/git checkout --";
      "gf"="${git}/bin/git fetch";
      "gi"="${git}/bin/git init";
      "gl"="${git}/bin/git log --graph --pretty=\"format:%C(yellow)%h%Creset %C(red)%G?%Creset%C(green)%d%Creset %s %Cblue(%cr) %C(bold blue)<%aN>%Creset\"";
      "gll"="${git}/bin/git log --pretty=\"format:%C(yellow)%h%Creset %C(red)%G?%Creset%C(green)%d%Creset %s %Cblue(%cr) %C(bold blue)<%aN>%Creset\"";
      "gL"="gl --stat";
      "gp"="${git}/bin/git push";
      "gpl"="${git}/bin/git pull --rebase --autostash";
      "gs"="${git}/bin/git status --short .";
      "gss"="${git}/bin/git status";
      "gst"="${git}/bin/git stash";
      "gr"="${git}/bin/git reset HEAD";
      "grv"="${git}/bin/git rev-parse";
    };

    home.configFile = {
      "git/config".source = "${configDir}/git/config";
      "git/ignore".source = "${configDir}/git/ignore";
      "gh/config.yml".source = "${configDir}/gh/config.yml";
    };
  };
}
