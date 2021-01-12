{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.messengers.email;
in {
  options.modules.messengers.email = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ w3m mutt neomutt offlineimap msmtp notmuch ];
    services.offlineimap = {
      enable = true;
      path = with pkgs; [ bash pass neomutt ];
      onCalendar = "*:0/30"; # fetch mail every 30 minutes
    };

    environment.shellAliases = {
      mutt = "neomutt";
      mail = "neomutt";
      m = "neomutt";
    };

    home.configFile = {
      "offlineimap/config".text = ''
        [general]
        # List of accounts to be synced, separated by a comma.
        ui = ttyui
        # gmail could also be an account
        accounts = isnt

        # [Account gmail]
        # # Identifier for the local repository; e.g. the maildir to be synced via IMAP.
        # localrepository = gmail-local
        # # Identifier for the remote repository; i.e. the actual IMAP, usually non-local.
        # remoterepository = gmail-remote

        # [Repository gmail-local]
        # # OfflineIMAP supports Maildir, GmailMaildir, and IMAP for local repositories.
        # type = Maildir
        # # Where should the mail be placed?
        # localfolders = ~/.mail/gmail

        # [Repository gmail-remote]
        # # Remote repos can be IMAP or Gmail, the latter being a preconfigured IMAP.
        # type = Gmail
        # remoteuser = ${secrets.gmail.user}
        # remotepass = ${secrets.gmail.password}
        # realdelete = no
        # maxconnections = 3
        # sslcacertfile = /etc/ssl/certs/ca-certificates.crt

        [Account isnt]
        localrepository = isnt-local
        remoterepository = isnt-remote

        [Repository isnt-local]
        type = Maildir
        ssl=yes
        localfolders = ~/.mail/isnt

        [Repository isnt-remote]
        type = IMAP
        remotehost = ${secrets.email.host}
        remoteuser = ${secrets.email.user}
        remotepass = ${secrets.email.password}
        realdelete = no
        maxconnections = 3
        sslcacertfile = /etc/ssl/certs/ca-certificates.crt
      '';
      "mutt/muttrc".text = ''
        set mbox_type   = Maildir
        set sendmail    = "/usr/bin/env msmtp"

        set folder      = ~/.mail/isnt
        set spoolfile   = "+INBOX"
        set mbox        = "+[isnt]/All Mail"
        set postponed   = "+[isnt]/Drafts"
        unset record

        mailboxes +INBOX

        macro index D \
            "<save-message>+[Gmail]/Trash<enter>" \
            "move message to the trash"

        macro index S \
            "<save-message>+[Gmail]/Spam<enter>" \
            "mark message as spam"

        # main options
        set realname   = "${secrets.name}"
        set from       = "${secrets.email.user}"
        set mail_check = 0
        set envelope_from

        unset move           # gmail does that
        set delete           # don't ask, just do
        unset confirmappend  # don't ask, just do!
        set quit             # don't ask, just do!!
        unset mark_old       # read/new is good enough for me

        # sort/threading
        set sort     = threads
        set sort_aux = reverse-last-date-received
        set sort_re

        # look and feel
        set reverse_name = yes
        set reverse_realname = yes

        set pager_index_lines = 8
        set pager_context     = 5
        set pager_stop
        set menu_scroll
        set smart_wrap
        set tilde
        unset markers

        # Status Bar
        set status_chars  = " *%A"
        set status_format = "───[ Folder: %f ]───[%r%m messages%?n? (%n new)?%?d? (%d to delete)?%?t? (%t tagged)? ]───%>─%?p?( %p postponed )?───"

        # Other binds to make mutt behave more like Vim, optional.
        bind pager j next-line
        bind pager k previous-line
        bind attach,index,pager \CD next-page
        bind attach,index,pager \CU previous-page
        bind pager g top
        bind pager G bottom
        bind attach,index g first-entry
        bind attach,index G last-entry

        # composing
        set fcc_attach
        unset mime_forward
        set forward_format = "Fwd: %s"
        set include
        set forward_quote

        ignore *                               # first, ignore all headers
        unignore from: to: cc: date: subject:  # then, show only these
        hdr_order from: to: cc: date: subject: # and in this order
      '';
      "msmtp/config".text = ''
        account default
        host ${secrets.email.host}
        port 587
        protocol smtp
        auth on
        from ${secrets.email.user}
        user ${secrets.email.user}
        password ${secrets.email.password}
        tls on
        tls_nocertcheck
      '';
    };
  };
}
