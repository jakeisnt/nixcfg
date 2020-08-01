{ config, lib, pkgs, ... }:

{
  xsession.windowManager.i3 = {
    enable = true;
    package = pkgs.i3-gaps;
    config = rec {
      modifier = "Mod4";
      focus.followMouse = false;
      window.titlebar = false;
      gaps = {
        smartGaps = true;
        outer = 0;
        inner = 15;
        top = 0;
      };

      startup = [{
        command = "systemctl --user restart polybar";
        always = true;
        notification = false;
      }];

      keybindings = { "key combo" = "bound cmd"; };
    };

    extraConfig = ''
      # definition of modifier key
      set $mod Mod4
      set $tab 23
      floating_modifier $mod

      # default monitor locations
      set $mon1 eDP-1
      set $mon2 DP-2
      set $mon3 DP-1

      # power
      bindsym $mod+Caps_Lock 		exec poweroff
      bindsym $mod+Shift+Caps_Lock 	exec reboot

      # run gui programs
      bindsym $mod+semicolon 		exec emacs
      bindsym $mod+apostrophe		exec firefox
      bindsym $mod+backslash 		exec spotify
      bindsym $mod+bracketright 	exec slack
      bindsym $mod+Return 		exec termite
      bindsym $mod+BackSpace 		exec thunar

      # arrow keys cheat
      # deprecated: do not need to cheat anymore
      # bindsym $mod+w exec xvkbd -xsendevent -text '\[Up]'
      # bindsym $mod+a exec xvkbd -xsendevent -text '\[Left]'
      # bindsym $mod+s exec xvkbd -xsendevent -text '\[Down]'
      # bindsym $mod+d exec xvkbd -xsendevent -text '\[Right]'
      # bindsym $mod+k exec xvkbd -xsendevent -text '\[Prior]'
      # bindsym $mod+j exec xvkbd -xsendevent -text '\[Next]'

      # Use pactl to adjust volume in PulseAudio.
      bindsym XF86AudioRaiseVolume 	exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ +10% && $refresh_i3status
      bindsym XF86AudioLowerVolume	exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ -10% && $refresh_i3status
      bindsym XF86AudioMute 		exec --no-startup-id pactl set-sink-mute @DEFAULT_SINK@ toggle && $refresh_i3status
      bindsym XF86AudioMicMute 	exec --no-startup-id pactl set-source-mute @DEFAULT_SOURCE@ toggle && $refresh_i3status

      # control music
      bindsym XF86AudioPrev 	exec playerctl previous
      bindsym $mod+comma 	exec playerctl previous
      bindsym XF86AudioPlay 	exec playerctl play-pause
      bindsym $mod+period 	exec playerctl play-pause
      bindsym XF86AudioNext 	exec playerctl next
      bindsym $mod+slash	exec playerctl next

      # adjust brightness
      bindsym XF86MonBrightnessUp 			exec light -A 10
      bindsym XF86MonBrightnessDown 			exec light -U 10
      bindsym Shift+XF86MonBrightnessUp 		exec light -A 5
      bindsym Shift+XF86MonBrightnessDown 		exec light -U 5
      bindsym Mod4+Shift+XF86MonBrightnessUp 		exec light -A 1
      bindsym Mod4+Shift+XF86MonBrightnessDown 	exec light -U 1

      # kill focused window
      bindsym $mod+Shift+q 	 kill
      bindsym $mod+XF86Display exec "multimonitor-output"

      bindsym $mod+i split h
      bindsym $mod+o split v

      # start rofi (a program launcher)
      bindsym $mod+v exec rofi -show run -lines 5 -bw 2 -width 20 -padding 50 -eh 2 -hide-scrollbar true

      # change focus
      bindsym $mod+h 		focus left
      bindsym $mod+Shift+j 	focus down
      bindsym $mod+Shift+k 	focus up
      bindsym $mod+l 		focus right

      bindsym $mod+q 		focus left
      bindsym $mod+e 		focus right

      bindsym $mod+Shift+h move left
      bindsym $mod+Shift+l move right

      bindsym $mod+n resize grow width 5 px or 5 ppt
      bindsym $mod+m resize shrink width 5 px or 5 ppt

      bindsym $mod+Shift+Tab 	workspace back_and_forth
      bindsym $mod+f 		fullscreen toggle


      # configure workspaces
      set $ws1  "1"
      set $ws2  "2"
      set $ws3  "3"
      set $ws4  "4"
      set $ws5  "5"
      set $ws6  "6"
      set $ws7  "7"
      set $ws8  "8"
      set $ws9  "9"
      set $ws10 "10"

      # switch to workspace
      bindsym $mod+1 workspace number $ws1
      bindsym $mod+2 workspace number $ws2
      bindsym $mod+3 workspace number $ws3
      bindsym $mod+4 workspace number $ws4
      bindsym $mod+5 workspace number $ws5
      bindsym $mod+6 workspace number $ws6
      bindsym $mod+7 workspace number $ws7
      bindsym $mod+8 workspace number $ws8
      bindsym $mod+9 workspace number $ws9
      bindsym $mod+0 workspace number $ws10

      # move focused container to workspace
      bindsym $mod+Shift+1 move container to workspace number $ws1
      bindsym $mod+Shift+2 move container to workspace number $ws2
      bindsym $mod+Shift+3 move container to workspace number $ws3
      bindsym $mod+Shift+4 move container to workspace number $ws4
      bindsym $mod+Shift+5 move container to workspace number $ws5
      bindsym $mod+Shift+6 move container to workspace number $ws6
      bindsym $mod+Shift+7 move container to workspace number $ws7
      bindsym $mod+Shift+8 move container to workspace number $ws8
      bindsym $mod+Shift+9 move container to workspace number $ws9
      bindsym $mod+Shift+0 move container to workspace number $ws10

      # with two monitors, send workspace 10 to the second
      workspace $ws10 output $mon2

      # with three monitors, send workspace 9 to the third
      workspace $ws9 output $mon3

      # restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
      bindsym $mod+r restart
      # exit i3 (logs you out of your X session)
      # bindsym $mod+Shift+x exec "i3-msg exit"

      # appearance
      for_window [class="^.*"] border pixel 1

      hide_edge_borders smart

      # popup configuration
      popup_during_fullscreen leave_fullscreen
      floating_minimum_size 300 x 300
      floating_maximum_size 3200 x 2400

      # color scheme
      # *client.<class>*      <border>  <bg>  <text>  <indic.>  <child_border>
      client.focused          #2257A0 #2257A0 #FFFFFF #2257A0   #51afef
      client.focused_inactive #333333 #2257A0 #FFFFFF #484E50   #5F676A
      client.unfocused        #333333 #303641 #888888 #292D2E   #1d2026
      client.urgent           #2F343A #ff6c6b #FFFFFF #CC241D   #CC241D
      client.placeholder      #000000 #0C0C0C #FFFFFF #000000   #0C0C0C
    '';
  };

  services.polybar = rec {
    enable = true;
    package = pkgs.polybar.override {
      i3GapsSupport = true;
      pulseSupport = true;
    };

    script = ''
      #!/usr/bin/env bash
      # check out github.com/woefe/dotfiles

      # Terminate already running bar instances
      killall -q polybar

      # Wait until the processes have been shut down
      while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

      polybar top
    '';

    extraConfig = ''
      [global/wm]
      margin-top = 0
      margin-bottom = 0

      [color]
      white = #b6bdc5
      base = #efebe0
      brown = #666
      orange = #fbb253
      orange2 = #ef7922
      purple = #e58a95
      red = #f1383d
      pink = #f26262
      pink2 = #f64d64
      blue = #51afef
      cyan = #51afef
      adapta = #1d2026

      [bar/top]
      override-redirect = false
      bottom = false
      fixed-center = true

      dpi = 200
      width = 100%
      height = 45

      font-0 = "Fira Code:size=8;0"
      font-1 = "Font Awesome 5 Free:style=Solid:size=10;1"

      background = ${color.adapta}
      foreground = ${color.white}

      line-color = ${color.cyan}
      underline-size = 0
      overline-size = 0

      border-bottom-size = 0
      border-bottom-color = ${color.cyan}

      padding-left = 1
      padding-right = 1
      module-margin-left = 2
      module-margin-right = 0

      tray-position = right
      tray-offset-y = -3
      tray-maxsize = 25
      tray-background = ${color.adapta}

      scroll-up = i3wm-wsnext
      scroll-down = i3wm-wsprev

      ## Mod Loc #########
      modules-left = bspwm
      modules-center = xwindow
      modules-right = xkeyboard music volume battery time date

      # Modules ##########
      [module/music]
      type = custom/script
      format =  <label>
      exec = spotify-info
      exec-if = spotify-info
      interval = 1<Paste>

      [module/xwindow]
      type = internal/xwindow
      format = <label>
      label = %title%
      label-maxlen = 75

      [module/xkeyboard]
      type = internal/xkeyboard
      blacklist-0 = num lock
      blacklist-1 = scroll lock

      format = <label-indicator>

      format-prefix =
      format-prefix-foreground = ${color.red}


      label-layout = %name%

      label-indicator-padding = 1
      label-indicator-margin = 0
      label-indicator-background = ${color.red}

      [module/i3]
      [module/battery]
      type = internal/battery
      battery = BAT0
      adapter = AC
      full-at = 98

      format-charging = <label-charging>  <animation-charging>
      format-discharging = <label-discharging>  <ramp-capacity>
      format-full = 
      format-full-foreground = ${color.cyan}

      ramp-capacity-0 = ${self.animation-charging-0}
      ramp-capacity-1 = ${self.animation-charging-1}
      ramp-capacity-2 = ${self.animation-charging-2}
      ramp-capacity-3 = ${self.animation-charging-3}
      ramp-capacity-4 = ${self.animation-charging-4}
      ramp-capacity-foreground-0-foreground = ${color.red}
      ramp-capacity-foreground = ${color.white}

      animation-charging-0 = 
      animation-charging-1 = 
      animation-charging-2 = 
      animation-charging-3 = 
      animation-charging-4 = 
      animation-charging-foreground = ${color.white}
      animation-charging-framerate = 750

      [module/volume]
      type = internal/pulseaudio

      format-volume = <ramp-volume>

      ramp-volume-0 = 
      ramp-volume-1 = 
      ramp-volume-2 = 
      format-foreground = ${color.cyan}

      format-muted = 
      format-muted-foreground = ${color.cyan}

      [module/date]
      type = internal/date
      interval = 1

      date = "%Y-%m-%d"

      label = %date%

      [module/time]
      type = internal/date
      interval = 1

      time = %H:%M

      label = %time%
    '';
  };
}
