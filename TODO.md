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

---

# GitHub Issues Backlog

## System / Boot

- [ ] **[#14 Set up encrypted boot](https://github.com/jakeisnt/nixcfg/issues/14)**
  My disk isn't at all encrypted on boot now. This isn't a huge priority issue but it would be helpful to encrypt everything, especially on my VPS - where the provider has the potential to access all of my data if it isn't.

- [ ] **[#15 Set up ZFS](https://github.com/jakeisnt/nixcfg/issues/15)**
  ext4 is pretty fast, but it would be great to gain some experience with more advanced file systems. Supposedly ZFS has excellent NixOS support; figure out how to get it working! `Vultr` is running with btrfs already, but oracle isn't very nice.

- [ ] **[#109 Start all programs in systemd cgroups](https://github.com/jakeisnt/nixcfg/issues/109)**
  why? idk seems fun — you can play and pause all programs then :')

## Networking / Services

- [ ] **[#103 Create internal DNS network for personal use servers](https://github.com/jakeisnt/nixcfg/issues/103)**
  https://christine.website/blog/series/site-to-site-wireguard and other blog posts of hers cover this

- [ ] **[#78 Use declarative backup service](https://github.com/jakeisnt/nixcfg/issues/78)**
  duplicati is nice but the backup information is declared dynamically through a web gui rather than beforehand in a config file. the latter would be nice

## Shell / Dev Environment

- [ ] **[#194 Minimal and maximal shells](https://github.com/jakeisnt/nixcfg/issues/194)**
  No reason to install all the cool rust stuff on the server.

- [ ] **[#196 Create a portable, use-anywhere dev shell](https://github.com/jakeisnt/nixcfg/issues/196)**
  1. Create a way to manage dotfiles outside of a home-manager configuration; this means creating and building configurations for programs into their packages rather than managing the files external to the packages. This is an elegant approach, but it might be super difficult to refactor with this config.
     - https://github.com/bouk/b/blob/master/fish.nix does this, pulling all deps into a custom fish package build
  2. Expose this through `default.nix`; can check `lib.inNixShell` to expose the shell or the configuration depending on the dev environment.
  3. Add a redirect to main branch `.tar.gz` of this repo with nginx on the isnt.online server if the user has a particular user agent: https://serverfault.com/questions/775463/nginx-redirect-based-on-user-agent.

- [ ] **[#217 Script(s) to set up necessarily stateful deps on shell start](https://github.com/jakeisnt/nixcfg/issues/217)**

- [ ] **[#220 `note` command](https://github.com/jakeisnt/nixcfg/issues/220)**
  Usage: `note the quick brown fox jumped over the lazy dog`
  Behavior: Append to the wiki page for the current day with a heading, timestamp and body, then commit to git with that note.

- [ ] **[#225 More fzf commands](https://github.com/jakeisnt/nixcfg/issues/225)**
  https://github.com/nushell/nushell/issues/5785
  - nushell integrated wifi selection
  - better git integration
  - find website by fzfing through browser history (visit ....)
  - and more!

- [ ] **[#213 tree-sitter-nushell](https://github.com/jakeisnt/nixcfg/issues/213)**
  This repo exists — https://github.com/LhKipp/tree-sitter-nu — but not sure if there is a newer one.

## Desktop / Window Manager

- [ ] **[#218 'Create a copy of this window' keyboard shortcut](https://github.com/jakeisnt/nixcfg/issues/218)**
  Should work on any window and preserve all of that window's state. Deep clone.

- [ ] **[#223 Language to create panels and open windows, restoring a 'workspace'](https://github.com/jakeisnt/nixcfg/issues/223)**
  For work, I want to:
  - open chrome with specific tabs (tabs can be pinned)
  - open emacs to a specific folder
  - make certain commands and environment variables available
  - start a development client and server with my current work

  Alternatively, restore the previous state of my current workspace — need to distinguish 'workspace modification' commands from 'active work commands'.

- [ ] **[#229 Implement workspace groups](https://github.com/jakeisnt/nixcfg/issues/229)**
  https://github.com/cjbassi/i3-workspace-groups — associate them with a folder on the desktop that has some settings, direnv, and a nix flake. So everything you want in the group is available when you're working. Also associate a web browser session and an emacs workspace (?) with the group.

- [ ] **[#227 Way to declare shell-focused command](https://github.com/jakeisnt/nixcfg/issues/227)**
  - Use alacritty to run command in terminal, opening a window to interact with
  - Way to specify the width and height of the new window you open
  - Specify the title of the window (or id, class) so it can be queried by other systems

- [ ] **[#232 A way to open up a website in a mini browser window without browser](https://github.com/jakeisnt/nixcfg/issues/232)**
  `runsite https://tool.com` → shows window without any browser adornment or interaction so I can focus on using the tool → fullscreen styling, but only occupies part of the page - small window with specific width and height

- [ ] **[#215 Show errors from some hidden commands as popups](https://github.com/jakeisnt/nixcfg/issues/215)**

- [ ] **[#209 Show whether trackpad is disabled or not in toolbar](https://github.com/jakeisnt/nixcfg/issues/209)**

## Hardware / Input

- [ ] **[#200 Colemak on framework laptop](https://github.com/jakeisnt/nixcfg/issues/200)**
  Detailed instructions here for using a firmware mod and a systemd service:
  https://community.frame.work/t/changed-my-keyboard-layout-in-hardware-to-colemak/14174/5
  Not sure how this will interact with an external keyboard though — does changing the firmware remap only the laptop's device keys, or also external keyboards?

- [ ] **[#206 Keybind for toggling touchpad/mouse on framework](https://github.com/jakeisnt/nixcfg/issues/206)**

- [ ] **[#226 Lock screen on lid close](https://github.com/jakeisnt/nixcfg/issues/226)**
  https://forums.fedoraforum.org/showthread.php?291877-Run-a-script-when-laptop-lid-opens-closes

## Display / Fonts

- [ ] **[#233 Use good font for startup sequence](https://github.com/jakeisnt/nixcfg/issues/233)**
  - Convert ttf to bitmap at a decent size (20px?) for terminal
  - Build that font into nix config (somehow)

- [ ] **[#236 Improve font rendering](https://github.com/jakeisnt/nixcfg/issues/236)**
  To get better font rendering on Linux, enable stem darkening by adding this to your `/etc/environment` file:
  ```
  FREETYPE_PROPERTIES="cff:no-stem-darkening=0 autofitter:no-stem-darkening=0"
  ```

- [ ] **[#35 Fix image previews in terminal](https://github.com/jakeisnt/nixcfg/issues/35)**
  In Alacritty… there are some X11 terminals that work, but the DPI scaling is terrible with XWayland!
  - Find a terminal with proper wayland and images support
  - Find an image viewer that successfully previews those images
  - Configure them to work!

- [ ] **[#207 Screenshot tool needs a file path?](https://github.com/jakeisnt/nixcfg/issues/207)**
  `printscr` doesn't work as intended right now

## Programs / Applications

- [ ] **[#235 Upgrade some programs](https://github.com/jakeisnt/nixcfg/issues/235)**
  https://omakub.org/
  - https://flameshot.org/
  - https://www.pinta-project.com/

- [ ] **[#96 Configure thunderbird](https://github.com/jakeisnt/nixcfg/issues/96)**
  I have a somewhat complex thunderbird configuration but it has to be redone manually. Codify it.

- [ ] **[#169 Improve firefox configuration](https://github.com/jakeisnt/nixcfg/issues/169)**
  https://github.com/pyllyukko/user.js#installation

- [ ] **[#219 Upgrade `weather` command](https://github.com/jakeisnt/nixcfg/issues/219)**
  https://www.reddit.com/r/unixporn/comments/jp5nix/xterm_v3wttrin_weather_in_the_region/

- [ ] **[#230 Parallel youtube backups](https://github.com/jakeisnt/nixcfg/issues/230)**
  https://wejn.org/2023/02/parallelize-youtube-downloads/
