{ pkgs, ... }:

{
  programs.bash = { enable = false; };
  programs.zsh = {
    enable = true;
    autocd = true;
    shellAliases = {
      mkdir = "mkdir -p";
      ga = "git add .";
      weather = "curl wttr.in";
    };

    # like shell aliases, but substituted anywhere on a line
    shellGlobalAliases = { mkdir = "mkdir -p"; };
    enableCompletion = true;
    enableAutosuggestions = true;
    history.extended = true;

    # same format as aliases
    # env variables for zsh sessions
    sessionVariables = { };

    plugins = [

    ];

    oh-my-zsh = {
      enable = true;
      plugins = [ "extract" "fzf" "git" "magic-enter" ];
      theme = "evan";
      extraConfig = ''
        MAGIC_ENTER_GIT_COMMAND='git status -u .'
        MAGIC_ENTER_OTHER_COMMAND='ls -lh .'
      '';
    };
  };

  # allows completion for system packages
  # environment.pathsToLink = [ "/share/zsh" ];
}
