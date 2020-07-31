{ config, lib, pkgs, ... }:

{
  programs.zathura = {
    enable = true;
    notification-error-bg = "#586e75"; # base01  # seem not work
    notification-error-fg = "#dc322f"; # red
    notification-warning-bg = "#586e75"; # base01
    notification-warning-fg = "#dc322f"; # red
    notification-bg = "#586e75"; # base01
    notification-fg = "#b58900"; # yellow

    completion-group-bg = "#002b36"; # base03
    completion-group-fg = "#839496"; # base0
    completion-bg = "#263238"; # base02
    completion-fg = "#93a1a1"; # base1
    completion-highlight-bg = "#586e75"; # base01
    completion-highlight-fg = "#eee8d5"; # base2

    # Define the color in index mode
    index-bg = "#383c4a"; # base02
    index-fg = "#93a1a1"; # base1
    index-active-bg = "#586e75"; # base01
    index-active-fg = "#eee8d5"; # base2

    inputbar-bg = "#586e75"; # base01
    inputbar-fg = "#eee8d5"; # base2

    statusbar-bg = "#383c4a"; # base02
    statusbar-fg = "#93a1a1"; # base1

    highlight-color =
      "#657b83"; # base00  # hightlight match when search keyword(vim's /)
    highlight-active-color = "#268bd2"; # blue

    default-bg = "#383c4a"; # base02
    default-fg = "#93a1a1"; # base1
    # set render-loading              true
    # set render-loading-fg           "#263238" # base02
    # set render-loading-bg           "#263238" # base02

    # Recolor book content's color
    recolor = false;
    recolor-lightcolor = "#383c4a"; # base02
    recolor-darkcolor = "#93a1a1"; # base1
    recolor-keephue = true; # k;eep original color

    # copy to clipboard
    selection-clipboard = "clipboard";
    extraConfig = ''
      map r reload
      map R rotate
      map K zoom in
      map J zoom out
      map c recolor
      map b toggle_statusbar
    '';
  };

}
