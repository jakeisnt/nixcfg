{ config, lib, pkgs, ... }:

{
  services.dunst = {
    enable = true;
    settings = {
      global = {
        monitor = 0;
        geometry = "800x600-0+0";
        indicate_hidden = "yes";
        shrink = "no";
        separator_height = 0;
        padding = 20;
        horizontal_padding = 20;
        frame_width = 4;
        sort = "no";
        idle_threshold = 120;
        font = "rissole 8";
        line_height = 4;
        markup = "full";
        format = ''
          %s
          %b'';
        alignment = "left";
        show_age_threshold = 60;
        word_wrap = "yes";
        ignore_newline = "no";
        stack_duplicates = false;
        hide_duplicate_count = "yes";
        show_indicators = "no";
        icon_position = "off";
        sticky_history = "yes";
        history_length = 20;
        browser = "/usr/bin/env firefox";
        always_run_script = true;
        title = "Dunst";
        class = "Dunst";
      };

      shortcuts = {
        close = "ctrl+space";
        close_all = "ctrl+shift+space";
        history = "ctrl+grave";
        context = "ctrl+shift+period";
      };

      urgency_low = {
        timeout = 2;
        background = "#383c4a";
        foreground = "#ffffff";
        frame_color = "#00bcd4";
      };

      urgency_normal = {
        timeout = 2;
        background = "#383c4a";
        foreground = "#ffffff";
        frame_color = "#ba68c8";
      };

      urgency_critical = {
        timeout = 2;
        background = "#383c4a";
        foreground = "#ffffff";
        frame_color = "#ff7043";
      };
    };
  };
}
