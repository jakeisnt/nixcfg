{ lib, config, pkgs, inputs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.mailserver;
in {
  options.modules.services.mailserver = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # nixpkgs.overlays = [ inputs.simple-nixos-mailserver ];
    mailserver = {
      enable = true;
      fqdn = "mx.isnt.online";
      domains = [ "isnt.online" ];

      # A list of all login accounts. To create the password hashes, use
      loginAccounts = {
          "jake@isnt.online" = {
            hashedPassword = "$2y$05$i2n.i1k9b1JIOKhokTI2YeQSY93rZG5gneBF32cT6X7EhxI2t/bJS";
            aliases = [ ];

              # Make this user the catchAll address for domains example.com and
              # example2.com
              catchAll = [
                  "isnt.online"
              ];
          };
      };

      # Extra virtual aliases. These are email addresses that are forwarded to
      # loginAccounts addresses.
      extraVirtualAliases = {
          # address = forward address;
      "jake2@isnt.online" = "jake@isnt.online";
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
