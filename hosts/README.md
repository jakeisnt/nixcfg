# Host configurations

These are the configurations preserved in this archived snapshot:

| Target | Platform | Purpose | Main modules |
| --- | --- | --- | --- |
| `work` | NixOS `x86_64-linux` | Framework laptop | Sway, Firefox/Chrome, audio, recording, AI development, Nushell, Git, GnuPG, direnv, SSH, Tailscale |
| `xps` | NixOS `x86_64-linux` | Dell XPS 9370 | Sway, Firefox, graphics/documents/recording, RSS/Signal/WeeChat, Vim/VS Code, AI/Node, Docker, Syncthing, SSH, DNSMasq, Bluetooth/scanner/printer |
| `iso-install` | NixOS `x86_64-linux` | Installer ISO | Vim, Fish, Git, file utilities, NetworkManager |
| `mac` | nix-darwin `aarch64-darwin` | Apple Silicon Mac | Fish, Git, Node, Rust, ripgrep, fzf, jq |

All hosts share `hosts/personal.nix` where appropriate and the reusable modules
under `modules/`. The flake checks the three Linux targets on Linux and the Mac
target on Apple Silicon; use `hey check` to run the native audit.
