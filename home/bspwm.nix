{ config, lib, pkgs, ... }:

{
  xsession.windowManager.bspwm = {
    enable = true;
    monitors = {
      "DP-1" = [ "1" "2" "3" ];
      "eDP-1" = [ "4" "5" "6" "7" ];
      "DP-2" = [ "8" "9" "0" ];
    };

    rules = {

    };

    settings = {
      "border_width" = 2;
      "window_gap" = 20;
      "top_padding" = 0;
      "left_padding" = 0;
      "right_padding" = 0;
      "bottom_padding" = 0;
      "split_ratio" = 0.52;

      "borderless_monocle" = true;
      "gapless_monocle" = true;
      "focus_follows_pointer" = false;
      "pointer_follows_focus" = false;
      "click_to_focus" = true;

      "pointer_action1" = "move";
      "pointer_action2" = "resize_side";
      "pointer_action3" = "resize_corner";

      "presel_feedback_color" = "#1d2026";
    };

    extraConfig = ''
      # TODO: https://www.reddit.com/r/bspwm/comments/ggtwxa/guide_to_creating_startup_layout_using_receptacles/
      # https://github.com/windelicato/dotfiles/wiki/bspwm-for-dummies
      # https://www.reddit.com/r/unixporn/comments/gftc5o/oc_realtime_websearch_suggestions_for_rofi_extras/

      sxhkd &

      # idea: named desktops for'dev', 'chat', 'editing', etc

      # add hidden scratchpad
      bspc rule -a dropdown sticky=on state=floating locked=on hidden=on
      termite --class dropdown -e "zsh -i"

      bspc rule -a keys-help sticky=on state=floating
      bspc rule -a Emacs sticky=off state=tiled

      bspswallow &
    '';
  };

  services.sxhkd = {
    enable = true;
    extraConfig = ''
      # make sxhkd reload its configuration files:
      # pkill -USR1 -x sxhkd

      # --- run programs ---

      # terminal emulator
      super + Return
      	termite

      # open scratchpad
      super + w
          scratch dropdown

      # toggle conky
      # TODO: multiple conky panels
      super + p
          toggle-conky

      # program launcher
      super + v
          exec rofi -show run -lines 5 -bw 2 -width 20 -padding 50 -eh 2 -hide-scrollbar true

      # start firefox
      super + apostrophe
          exec firefox


      # power off or reboot system
      super + {_,shift +}Caps_Lock
          exec {poweroff,reboot}


      # --- navigation ---

      # rotate desktop
      super + {_, shift + }r
          bspc node @/ --rotate {90,-90}

      # circulate leaves of tree
      super + {_, shift +}c
          bspc node @/ --circulate {backward,forward}

      # make split ratios equal
      super + equal
          bspc node @/ --equalize

      # make split ratios balanced
      super + minus
          bspc node @/ --balance

      # close window
      super + shift + q
          bspc node -c

      # send the newest marked node to the newest preselected node
      super + y
      	bspc node newest.marked.local -n newest.!automatic.local

      # swap the current node and the biggest node
      super + g
      	bspc node -s biggest

      # state/flags

      # toggle state of current window
      super + {t,shift + t,s,f}
      	bspc node --state \~{tiled,pseudo_tiled,floating,fullscreen} # fullscreen

      # TODO: change state of all nodes on workspace (with super + alt + something)

      # set the node flags
      super + ctrl + {m,x,y,z}
      	bspc node -g {marked,locked,sticky,private}

      # focus/swap

      # focus the node in the given direction
      super + {_,shift + }{h,j,k,l}
          bspc node -{f,s} {west,south,north,east}

      # focus the node for the given path jump
      super + {p,b,comma,period}
      	bspc node -f @{parent,brother,first,second}

      # focus the next/previous node in the current desktop
      super + {_,shift + }c
      	bspc node -f {next,prev}.local

      # focus the next/previous desktop in the current monitor
      super + bracket{left,right}
      	bspc desktop -f {prev,next}.local

      # focus the last node/desktop
      super + {grave,Tab}
      	bspc {node,desktop} -f last

      # focus the previous / next window
      super + {q,e}
          bspc node --focus {prev,next}

      # focus the older or newer node in the focus history
      super + {u,i}
      	bspc wm -h off; \
      	bspc node {older,newer} -f; \
      	bspc wm -h on

      # move window to last split
      super + y
      	bspc query --nodes -n focused.automatic \
      	&& bspc node -n last.\!automatic \
      	|| bspc node last.leaf -n focused

      # focus or send to the given desktop
      super + {_,shift + }{1-9,0}
      	bspc {desktop -f,node -d} '^{1-9,0}'

      # allows for multiple desktops on the same monitors
      # TODO still in progress: not currently working
      # super + {_,shift + } {1,2,3} ; {1-9,0}
      	# notify-send "changing desktop"; \
      	# bspc {desktop -f, node -d} "^{1,2,3}:^{1-9,10}"

      # focus any desktop on the currently focused monitor
      # TODO still wip
      # alt + {1-9,0}
      #     focus-desktop-to-cur-monitor {1-9,0}

      # alt + shift + {1-9,0}
          # place-win-on-desktop {1-9,0}
          # TODO

      # grab last window and swap it with current one, keeping focus
      super + apostrophe
          swap-windows

      #
      # --- preselect ---
      #

      # preselect the direction
      super + ctrl + {h,j,k,l}
      	bspc node -p {west,south,north,east}

      # preselect the ratio
      super + ctrl + {1-9}
      	bspc node -o 0.{1-9}

      # cancel the preselection for the focused node
      super + ctrl + space
      	bspc node -p cancel

      # cancel the preselection for the focused desktop
      super + ctrl + shift + space
      	bspc query -N -d | xargs -I id -n 1 bspc node id -p cancel

      #
      # --- move/resize ---
      #

      # expand a window by moving one of its side outward
      # TODO: different behavior when floating
      super + alt + {h,j,k,l}
      	bspc node -z {left -20 0,bottom 0 20,top 0 -20,right 20 0}

      # contract a window by moving one of its side inward
      # TODO: different behavior when flaoting
      super + alt + shift + {h,j,k,l}
      	bspc node -z {right -20 0,top 0 20,bottom 0 -20,left 20 0}

      # move a floating window TODO

      # show help with key bindings
      # TODO needs some reformatting or something
      # TODO should also be in floating terminal
      super + colon
          termite --class keys-help -e keys-help

      # --- system settings ---
      # quit/restart bspwm
          super + alt + {q,r}
          bspc {quit,wm -r}

      # --- audio
      # raise volume
      XF86AudioRaiseVolume
          exec pulseaudio-ctl up 10
      # lower volume
      XF86AudioLowerVolume
          exec pulseaudio-ctl down 10
      # mute volume
      XF86AudioMute
          exec pulseaudio-ctl mute

      # --- music
      # go to previous song
      XF86AudioPrev
          exec playerctl previous
      # go to previous song
      super + comma
          exec playerctl previous
      # play or pause music
      XF86AudioPlay
          exec playerctl play-pause
      # play or pause music
      super + period
          exec playerctl play-pause
      # go to next song
      XF86AudioNext
          exec playerctl next
      # go to next song
      super + slash
          exec playerctl next

      # --- brightness
      # raise brightness
      XF86MonBrightnessUp
          exec light -A 10
      # lower brightness
      XF86MonBrightnessDown
          exec light -U 10
      # raise brightness by smaller increment
      shift + XF86MonBrightnessUp
          exec light -A 5
      # lower brightness by smaller increment
      shift + XF86MonBrightnessDown
          exec light -U 5
      # raise brightness by small increment
      super + shift + XF86MonBrightnessUp
          exec light -A 1
      # lower brightness for small increment
      super + shift + XF86MonBrightnessDown
          exec light -U 1
    '';
  };
}
