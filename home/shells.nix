{ pkgs, ... }:
let
  aliases = {
    mkdir = "mkdir -p";
    weather = "curl wttr.in";
  };
  globalAliases = { mkdir = "mkdir -p"; };
in {
  programs.bash = {
    enable = true;
    shellAliases = aliases;
  };

  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      set fish_greeting
      fish_vi_key_bindings
      bind -M insert -m default jk backward-char force-repaint
    '';

  };

  programs.zsh = {
    enable = true;
    autocd = true;
    shellAliases = aliases;

    # like shell aliases, but substituted anywhere on a line
    shellGlobalAliases = globalAliases;
    enableCompletion = true;
    enableAutosuggestions = true;

    history = {
      extended = true;
      ignoreSpace = true;
      share = true;
    };

    # same format as aliases
    # env variables for zsh sessions
    sessionVariables = { };

    plugins = [{
      name = "nix-zsh-completions";
      src = pkgs.fetchFromGitHub {
        owner = "spwhitt";
        repo = "nix-zsh-completions";
        rev = "v0.4.4";
        sha256 = "1n9whlys95k4wc57cnz3n07p7zpkv796qkmn68a50ygkx6h3afqf";
      };
    }];

    oh-my-zsh = {
      enable = true;
      plugins = [ "extract" "git" "magic-enter" "npm" ];
      theme = "evan";
      extraConfig = ''
        MAGIC_ENTER_GIT_COMMAND='git status -u .'
        MAGIC_ENTER_OTHER_COMMAND='ls -lh .'

        eval "$(direnv hook zsh)"
        # eval "$(starship init zsh)"
      '';
    };
  };

  # allows completion for system packages
  # environment.pathsToLink = [ "/share/zsh" ];
}
