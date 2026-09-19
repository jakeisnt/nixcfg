{ config, options, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.git;
in {
  options.modules.shell.git = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      [
        git
        gitflow
        gh
        git-open
        diff-so-fancy
        git-absorb
        # semantic diff!
        difftastic
        # good diff page viewer
        delta
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
      "git/config".source = mkOutOfStoreSymlink "${configDir}/git/config";
      # This may be a symlink left by an older checkout. Home Manager cannot
      # back up a conflicting symlink, so replacing the link is intentional.
      "git/ignore" = {
        source = mkOutOfStoreSymlink "${configDir}/git/ignore";
        force = true;
      };
    };

    # `gh auth login` updates config.yml (including when it migrates older
    # versions).  It cannot do that while Home Manager points the file at this
    # repository, so use the checked-in file only to seed a mutable copy.
    # The symlink-to-file conversion also migrates existing installations on
    # their next rebuild without touching an already mutable configuration.
    home-manager.users.${config.user.name}.home.activation.ghConfig =
      inputs.home-manager.lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        gh_config_dir="$XDG_CONFIG_HOME/gh"
        gh_config="$gh_config_dir/config.yml"

        $DRY_RUN_CMD mkdir -p "$gh_config_dir"
        if [ -L "$gh_config" ]; then
          $DRY_RUN_CMD rm "$gh_config"
          $DRY_RUN_CMD cp ${escapeShellArg "${configDir}/gh/config.yml"} "$gh_config"
        elif [ ! -e "$gh_config" ]; then
          $DRY_RUN_CMD cp ${escapeShellArg "${configDir}/gh/config.yml"} "$gh_config"
        fi
        # The template is copied from the read-only Nix store.  `cp` preserves
        # its mode, so explicitly restore user write access after seeding (and
        # repair copies created by older versions of this activation hook).
        if [ -f "$gh_config" ] && [ ! -L "$gh_config" ]; then
          $DRY_RUN_CMD chmod u+w "$gh_config"
        fi
      '';
  };
}
