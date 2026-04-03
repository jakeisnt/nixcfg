# Darwin Integration TODOs

## Bootstrap (do once)
- [ ] Run `nix flake update` to pull in the `nix-darwin` lock entry
- [ ] Install nix-darwin: `nix run nix-darwin -- switch --flake .#mac`
- [ ] Verify `darwin-rebuild switch --flake .#mac` succeeds

## Option layer
- [ ] `user` on Linux is `attrs`-typed and aliases to `users.users.jake`; on darwin it is structured (`packages`, `shell`). Any module that sets `user.extraGroups`, `user.uid`, `user.isNormalUser`, etc. will fail if imported on darwin — audit and guard those callsites when porting modules
- [ ] `direnv.nix` uses `environment.pathsToLink` (NixOS-only) — wrap in `mkIf pkgs.stdenv.isLinux`
- [ ] `nushell.nix` sets `users.defaultUserShell` when enabled — same fix as fish.nix (`mkMerge` with platform guards)
- [ ] `gnupg.nix` uses `programs.gnupg.agent.pinentryPackage = pkgs.emacs` — verify nix-darwin supports this or guard it; `tomb` is Linux-only

## Modules to port (add to darwin.nix imports once verified)
- [ ] `modules/shell/direnv.nix` — guard `environment.pathsToLink`
- [ ] `modules/shell/gnupg.nix` — guard pinentry, tomb
- [ ] `modules/shell/tmux.nix` — depends on `modules.theme.onReload` (theme module not imported on darwin); either stub the option or skip
- [ ] `modules/shell/bitwarden.nix` — check for Linux-only options
- [ ] `modules/dev/python.nix` — uses `pkgs.python37` (old, may not build on darwin); upgrade to current python
- [ ] `modules/dev/cc.nix`, `modules/dev/clojure.nix`, `modules/dev/lua.nix` — audit
- [ ] `modules/editors/vim.nix`, `modules/editors/vscode.nix` — likely safe, audit
- [ ] `modules/editors/emacs.nix` — uses `pkgs.emacs29-pgtk` (Linux/X11 only); needs darwin variant (`pkgs.emacs29` or `pkgs.emacs-macport`)
- [ ] `modules/services/syncthing.nix` — nix-darwin has a syncthing service, verify option paths match

## Modules to skip on darwin (Linux/Wayland-only)
- `modules/desktop/` — sway, wayfire, gnome (not applicable on macOS)
- `modules/wayland/` — Linux display protocol
- `modules/hardware/` — NixOS hardware management
- `modules/security.nix` — boot/kernel options
- `modules/xdg.nix` — partially safe but has X11 (`XAUTHORITY`) bits; port XDG env vars, drop the rest
- `modules/gaming/` — Linux GPU/steam specifics
- `modules/vm/` — Linux VM tooling
- `modules/services/docker.nix` — use colima on darwin instead

## Flake cleanup
- [ ] `lib/nixos.nix` still has `let system = "x86_64-linux"` at top — this shadow is harmless but confusing; consider removing it since `system` is passed via `attrs`
- [ ] The `self.overlay` used for Linux pkgs injects `my = self.packages."x86_64-linux"` — darwin pkgs in `mkDarwinHost` use `my = {}` as a stub; populate it with darwin-compatible custom packages from `packages/` as needed
- [ ] `overlays/x-clipboard.nix` is Wayland/Linux-only — should not be applied to darwin pkgs (currently it isn't, since darwin pkgs skip `self.overlays`, but document this)
- [ ] Add an `aarch64-darwin` entry to `packages.*` output if any custom packages are darwin-compatible

## Host config
- [ ] Fill out `hosts/darwin/mac/default.nix` with your actual mac preferences (editors, services, etc.)
- [ ] Add a `hosts/darwin/personal-darwin.nix` (analogue of `hosts/personal.nix`) for shared darwin settings once you have more than one mac host
- [ ] Set `networking.computerName` in the mac host if you want the Bonjour name to differ from `hostName`
