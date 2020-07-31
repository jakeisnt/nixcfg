{ config, lib, pkgs, ... }:

{
  programs.termite = {
    enable = true;
    allowBold = true;
    audibleBell = false;
    mouseAutohide = true;
    scrollOnOutput = false;
    fullscreen = true;
    scrollbackLines = 10000;
    optionsExtra = ''
      bold_is_bright = false
      cell_height_scale = 1.0
      cell_width_scale = 1.0
      clickable_url = true
      dynamic_title = true
      font = Source Code Pro 10

      # $BROWSER is used by default if set, with xdg-open as a fallback
      browser = xdg-open
      padding = 5
      border_width = 5
      roundness = 2.0

      # "system", "on" or "off"
      cursor_blink = system
      cursor_shape = block
      filter_unmatched_urls = true

      # "off", "left" or "right"
      scrollbar = off
    '';

    backgroundColor = "#282c34";
    foregroundColor = "#bbc2cf";
    highlightColor = "#2f2f2f";
    cursorColor = "#5B6268";
    colorsExtra = ''
      #cursor = #dcdccc
      #cursor_foreground = #dcdccc
      #foreground_bold = #ffffff

      # black
      color0 = #1B2229
      color8 = #1c1f24

      # red
      color1 = #ff6c6b
      color9 = #e987b5

      # green
      color2 = #4db5bd
      color10 = #98be65

      # yellow
      color3 = #ECBE7B
      color11 = #c678dd

      # blue
      color4 = #51afef
      color12 = #ecbe6a

      # magenta
      color5 = #c678dd
      color13 = #da8548

      # cyan
      color6 = #46D9FF
      color14 = #51afef

      # white
      color7 = #DFDFDF
      color15 = #5B6268

      selection_background = #21242b
      selection_foreground = #5B6268
    '';

    hintsBackgroundColor = "#282c34";
    hintsForegroundColor = "#bbc2cf";
    hintsPadding = 5;
    hintsRoundness = 2;
    hintsBorderWidth = 5;
  };

}
