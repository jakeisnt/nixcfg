{ config, lib, pkgs, ... }:

{
  services.mbsync = {
    enable = true;
    frequency = "2m";
  };

  programs.mbsync = {
    extraConfig = ''
      IMAPAccount gmail
      # Address to connect to
      Host imap.gmail.com
      User jakechvatal@gmail.com
      Pass nice try!
      # To store the password in an encrypted file use PassCmd instead of Pass
      # PassCmd "gpg2 -q --for-your-eyes-only --no-tty -d ~/.mailpass.gpg"
      #
      # Use SSL
      SSLType IMAPS
      # The following line should work. If get certificate errors, uncomment the two following lines and read the "Troubleshooting" section.
      CertificateFile /etc/ssl/certs/ca-certificates.crt
      #CertificateFile ~/.cert/imap.gmail.com.pem
      #CertificateFile ~/.cert/Equifax_Secure_CA.pem

      IMAPStore gmail-remote
      Account gmail

      MaildirStore gmail-local
      Subfolders Verbatim
      # The trailing "/" is important
      Path ~/.mail/gmail/
      Inbox ~/.mail/gmail/inbox

      Channel gmail
      Master :gmail-remote:
      Slave :gmail-local:
      # Exclude everything under the internal [Gmail] folder, except the interesting folders
      Patterns * ![Gmail]* "[Gmail]/Sent Mail" "[Gmail]/Starred" "[Gmail]/All Mail" "[Gmail]/DeltaChat"
      # Or include everything
      #Patterns *
      # Automatically create missing mailboxes, both locally and on the server
      Create Both
      # Save the synchronization state files in the relevant directory
      SyncState *

      # outlook
      IMAPAccount outlook
      Host outlook.office365.com
      Port 993
      User chvatal.j@northeastern.edu
      Pass nice try!
      AuthMechs PLAIN
      SSLType IMAPS
      SSLVersions TLSv1.2

      IMAPStore outlook-remote
      Account outlook

      MaildirStore outlook-local
      SubFolders Verbatim
      Path ~/.mail/neu/
      Inbox ~/.mail/neu/inbox/
    '';
  };

}

# mbsync.service
#[Unit]
# Description=Mailbox synchronization service

# [Service]
# Type=oneshot
# ExecStart=/usr/bin/mbsync -Va

# mbsync.timer
#
#[Unit]
# Description=Mailbox synchronization timer

# [Timer]
# OnBootSec=2m
# OnUnitActiveSec=30m
# Unit=mbsync.service

# [Install]
# WantedBy=timers.target
