{ pkgs, ... }:
{
  programs.tmux = {
    enable = true;
    keyMode= "vi";
    plugins = with pkgs.tmuxPlugins; [
      yank
    ];
    terminal = "tmux-256color";
    shortcut = "a";
    baseIndex = 1;
    escapeTime = 10;
    extraConfig = ''
      # yank
      set -g @override_copy_command 'clip'

      set -g mouse on

      set-option -g renumber-windows on

      setw -g xterm-keys on
      set -g set-titles on

      set -sg repeat-time 600 # increase repeat timeout
      set -s focus-events on

      bind \\ split-window -h -c '#{pane_current_path}'  # Split panes horizontal
      bind - split-window -v -c '#{pane_current_path}'  # Split panes vertically
      bind > swap-pane -D       # swap current pane with the next one
      bind < swap-pane -U       # swap current pane with the previous one

      # pane navigation
      bind -r h select-pane -L  # move left
      bind -r j select-pane -D  # move down
      bind -r k select-pane -U  # move up
      bind -r l select-pane -R  # move right

      # pane resizing
      bind -r H resize-pane -L 5
      bind -r J resize-pane -D 5
      bind -r K resize-pane -U 5
      bind -r L resize-pane -R 5
    '';
  };
}
