# Loosely based off Pure <https://github.com/sindresorhus/pure>

_strlen() { echo ${#${(S%%)1//$~%([BSUbfksu]|([FB]|){*})/}}; }

# fastest possible way to check if repo is dirty
prompt_git_dirty() {
  command -v git >/dev/null || return

  # check if we're in a git repo
  [[ "$(command git rev-parse --is-inside-work-tree 2>/dev/null)" == "true" ]] || return
  # check if it's dirty
  command test -n "$(git status --porcelain --ignore-submodules -unormal)" || return

  local r=$(command git rev-list --right-only --count HEAD...@'{u}' 2>/dev/null)
  local l=$(command git rev-list --left-only --count HEAD...@'{u}' 2>/dev/null)

  (( ${r:-0} > 0 )) && echo -n " %F{red}${r}-"
  (( ${l:-0} > 0 )) && echo -n " %F{green}${l}+"
  echo -n '%f'
}

## Hooks ###############################
prompt_hook_precmd() {
  vcs_info # get git info
  # Newline before prompt, except on init
  [[ -n $PROMPT_DONE ]] && print ""; PROMPT_DONE=1
}

## Initialization ######################
prompt_init() {
  # prevent percentage showing up
  # if output doesn't end with a newline
  export PROMPT_EOL_MARK=

  # prompt_opts=(cr subst percent)
  setopt promptsubst
  autoload -Uz add-zsh-hook
  autoload -Uz vcs_info

  add-zsh-hook precmd prompt_hook_precmd
  # Updates cursor shape and prompt symbol based on vim mode
  zle-keymap-select() {
    case $KEYMAP in
      vicmd)      PROMPT_SYMBOL="%F{green}« " ;;
      main|viins) PROMPT_SYMBOL="%(?.%F{magenta}.%F{yellow})λ " ;;
    esac
    zle reset-prompt
    zle -R
  }
  zle -N zle-keymap-select
  zle -A zle-keymap-select zle-line-init

  zstyle ':vcs_info:*' enable git
  zstyle ':vcs_info:*' use-simple true
  zstyle ':vcs_info:*' max-exports 2
  zstyle ':vcs_info:git*' formats ' %b'
  zstyle ':vcs_info:git*' actionformats ' %b (%a)'

  # show username@host if logged in through SSH
  if [[ -n $SSH_CONNECTION ]]; then
    prompt_username='%m '
    if [[ $(whoami) != jake ]]; then
      prompt_username="%n.$prompt_username"
    fi
  fi

  # if in TTY, configure TTY color scheme
  if [ "$TERM" = "linux" ]; then
    echo -en "\e]P03B4252" #black, nord1
    echo -en "\e]P1BF616A" #darkred, nord11
    echo -en "\e]P2A3BE8C" #darkgreen, nord14
    echo -en "\e]P3EBCB8B" #brown, nord13
    echo -en "\e]P481A1C1" #darkblue, nord9
    echo -en "\e]P5B48EAD" #darkmagenta, nord15
    echo -en "\e]P688C0D0F4" #darkcyan, nord8
    echo -en "\e]P7E5E9F0" #lightgrey, nord5
    echo -en "\e]P84C566A" #darkgrey, nord3
    echo -en "\e]P9BF616A" #red, nord11
    echo -en "\e]PAA3BE8C" #green, nord14
    echo -en "\e]PBEBCB8B" #yellow, nord13
    echo -en "\e]PC81A1C1" #blue, nord9
    echo -en "\e]PDB48EAD" #magenta, nord15
    echo -en "\e]PE8FBCBB" #cyan, nord7
    echo -en "\e]PFECEFF4" #white, nord6
    clear
  fi

  RPROMPT='%F{blue}%~%F{magenta}${vcs_info_msg_0_}$(prompt_git_dirty)%f'
  PROMPT='%F{magenta}${prompt_username}%f${PROMPT_SYMBOL:-$ }%f'
}

prompt_init "$@"
