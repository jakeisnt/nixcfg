{ pkgs, ... }:

{
  programs.bash = { enable = true; };
  programs.zsh = {
    enable = false;
    initExtra = ''
      # Enable powerlevel10k instant prompt
      # determines whether a command exists
      cmd_exists () {
          type "$1" &> /dev/null;
      }
      export ARCHFLAGS="-arch x86_64"
      export MANPATH="/usr/local/man:$MANPATH"
      export RANGER_LOAD_DEFAULT_RC="FALSE" # only load zsh once

      # TODO only add things to path if they exist
      typeset -U PATH path
      path=(
          "/usr/local/bin"
          "$HOME/.bin"
          "$HOME/.local/bin"
          "$HOME/bin"
          "$HOME/.node_modules/bin"
          "$HOME/.cabal/bin"
          "$HOME/.ghcup/bin"
          "$path[@]"
      )
      export PATH
      export JAVA_HOME=$JAVA_HOME:/usr/lib/jvm/java-8-openjdk/jre
      export QT_AUTO_SCREEN_SCALE_FACTOR=1 # qutebrowser scaling

      # TODO: better WSL compatibility
      antigen use oh-my-zsh
      antigen theme romkatv/powerlevel10k
      antigen bundles<<EOBUNDLES
      colored-man-pages
      magic-enter
      extract
      vi-mode
      tmux
      git
      pyenv
      lainiwa/zsh-manydots-magic
      zsh-users/zsh-syntax-highlighting
      zsh-users/zsh-completions
      zsh-users/zsh-autosuggestions
      zsh-users/zsh-history-substring-search
      jakechvatal/autoedit
      jakechvatal/p
      EOBUNDLES
      antigen apply
      fi

      # --- History ---
      HISTSIZE=10000 # long history
      SAVEHIST=9000
      HISTFILE=~/.zsh_history
      HISTCONTROL=ignoredups:erasedups # no duplicates in history
      MISTIGNORE="exit"

      # ZSH Settings
      HYPHEN_INSENSITIVE="true"            # _ and - are the same for autocomplete
      DISABLE_AUTO_UPDATE="true"           # disable omzsh auto update
      DISABLE_UNTRACKED_FILES_DIRTY="true" # faster repo status check
      setopt INC_APPEND_HISTORY            # add commands to history as they are entered
      setopt AUTO_CD                       # auto change directories
      setopt CORRECT                       # correct commands
      setopt MULTIOS                       # pipe to multiple outputs
      setopt NO_CLOBBER                    # str doesn't clobber
      setopt RC_EXPAND_PARAM               # expand arround vars
      setopt NO_CASE_GLOB                  # case insensitive glob
      setopt NUMERIC_GLOB_SORT             # sort globs by #
      setopt EXTENDED_GLOB                 # glob for more!

      # --- Aliases ---
      # always ensure that the right editor is used
      alias vi=$EDITOR
      alias vim=$EDITOR
      alias nvim=$EDITOR
      alias ec="emacs"
      alias sudo="sudo " # fix sudo for some commands
      alias spotify="/usr/bin/spotify --force-device-scale-factor = 2.5"
      alias distro='cat /etc/*-release'
      alias reload='source ~/.zshrc'
      alias weather='curl wttr.in'

      # sane shell commands
      alias mkdir='mkdir -p'  # mkdir always makes recursive directories
      alias -g DN='/dev/null' # easier

      # trying out fasd
      alias a='fasd -a'        # any
      alias s='fasd -si'       # show / search / select
      alias d='fasd -d'        # directory
      alias f='fasd -f'        # file
      alias sd='fasd -sid'     # interactive directory selection
      alias sf='fasd -sif'     # interactive file selection
      alias z='fasd_cd -d'     # cd, same functionality as j in autojump
      alias zz='fasd_cd -d -i' # cd with interactive selection

      # --- Keybindings ---
      # home, end for beginning and end of line
      bindkey '\e[1~' beginning-of-line
      bindkey '\e[4~' end-of-line

      # incremental search is elite!
      bindkey -M vicmd "/" history-incremental-search-backward
      bindkey -M vicmd "?" history-incremental-search-forward

      # search based on what you typed in already
      bindkey -M vicmd "//" history-beginning-search-backward
      bindkey -M vicmd "??" history-beginning-search-forward

      # --- Startup ---
      cmd_exists opam && # ocaml support
          eval $(opam env)

      cmd_exists npm && # redirect node_modules
          export NPM_CONFIG_PREFIX=~/.node_modules

      cmd_exists fasd &&
          eval "$(fasd --init auto)"

      cmd_exists bspwm && # bspwm-specific scripts
          export PATH=$PATH:"$HOME/.config/bspwm/scripts"

      cmd_exists emacs &&
          export PATH=$PATH:~/.emacs.d/bin

      test -f "$HOME/.private" && # add local config if it exists
          source $HOME/.private


      # startx if tty1, display and has x
      if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
          exec startx
      fi

      # start prompt
      [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

      zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
      zstyle :compinstall filename '/home/jake/.zshrc'

      autoload -Uz compinit
      compinit
    '';

  };
}
