{ lib, config, pkgs, inputs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.mailserver;
    domain = config.networking.domain;
in {
  options.modules.services.mailserver = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [
      # Email
      25   # SMTP
      465  # Submission TLS
      587  # Submission StartTLS
      993  # IMAP with TLS
      995  # POP3 with TLS
      143  # IMAP with StartTLS
      110  # POP3 with StartTLS
    ];

    mailserver = {
      enable = true;
      fqdn = "mx.${domain}";
      domains = [ domain ];

      loginAccounts = {
        "jake@${domain}" = {
          hashedPassword = secrets.email.hashedPassword;
          aliases = [ ];
          catchAll = [ domain ];
        };
      };

      # Use Let's Encrypt certificates. Note that this needs to set up a stripped
      # down nginx and opens port 80.
      certificateScheme = 3;

      # Enable IMAP and POP3
      enableImap = true;
      enablePop3 = true;
      enableImapSsl = true;
      enablePop3Ssl = true;

      # Enable the ManageSieve protocol
      enableManageSieve = true;

      # whether to scan inbound emails for viruses (note that this requires at least
      # 1 Gb RAM for the server. Without virus scanning 256 MB RAM should be plenty)
      virusScanning = false;
    };

    # Accept LetsEncrypt certificates
    security.acme = {
      email = "jakechvatal@gmail.com";
      acceptTerms = true;
    };
  };
}
