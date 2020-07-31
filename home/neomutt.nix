{ config, lib, pkgs, ... }:

{
  programs.neomutt = {
    enable = true;
    vimKeys = true;

    extraConfig = ''
      # personality
      set realname = "Jacob Chvatal"
      set from = "jakechvatal@gmail.com"

      # IMAP
      # connection
      set mail_check=60
      set imap_keepalive=300
      # account
      set imap_user="jakechvatal@gmail.com"
      set imap_pass="nice try"
      set folder=imaps://jakechvatal@imap.gmail.com
      set spoolfile=+INBOX
      set record="+[Gmail]/Sent Mail"
      set postponed="[Gmail]/Drafts"
      set mbox="imaps://imap.gmail.com/[Gmail]/All Mail"

      # SMTP
      set smtp_url = "smtp://jakechvatal@smtp.gmail.com:587/"
      set smtp_pass = "$imap_pass"
      set smtp_authenticators = "login"
      # set ssl_force_tls = yes

      set mbox="imaps://imap.gmail.com/[Gmail]/All Mail"
      set postponed =  "[Gmail].All Mail"

      set spoolfile=+INBOX

      # settings
      set send_charset="utf-8"
      set assumed_charset="iso-8859-1"
      set editor="nvim -c 'set syntax=mail ft=mail enc=utf-8'"

      auto_view application/pdf
      auto_view application/msword
      auto_view text/html
      alternative_order text/plain text/enriched text/html

      set menu_scroll=yes
      # sidebar visibility
      set sidebar_visible=yes
      set sidebar_width=15
      set sidebar_folder_indent = yes
      set time_inc=300

      # color scheme
      # http://aperiodic.net/phil/configs/mutt/colors
      color normal      white          black
      color hdrdefault  green          default
      color quoted      green          default
      color quoted1     yellow         default
      color quoted2     red            default
      color signature   cyan           default
      color indicator   brightyellow   red
      color error       brightred      default
      color status      brightwhite    blue
      color tree        brightmagenta  black
      color tilde       blue           default
      color attachment  brightyellow   default
      color markers     brightred      default
      color message     white          black
      color search      brightwhite    magenta
      color bold        brightyellow   default
      # if you don't like the black progress bar at the bottom of the screen,
      # comment out the following line
      color progress    white          black
    '';
  };

}
