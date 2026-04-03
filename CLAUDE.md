# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Common Commands

The default shell is nushell. Use the `hey` script (in `bin/`) for common NixOS operations:

```nu
# Build and switch to current host configuration
hey rebuild
# (expands to: sudo nixos-rebuild switch --flake /etc/nixos --option pure-eval no)

# Update flake inputs and rebuild
hey upgrade

# Garbage collect old generations
hey gc
# (expands to: nix-collect-garbage -d)

# Find a package by name
hey find <package_name>
```

For one-off nix commands (run from `/etc/nixos`):

```nu
# Build without switching
sudo nixos-rebuild build --flake /etc/nixos --option pure-eval no

# Build the live USB ISO
nix build .#nixosConfigurations.iso-install.config.system.build.isoImage --option pure-eval no

# Check flake validity
nix flake check

# Enter dev shell (drops into bash; use direnv for nushell-native dev envs)
nix develop

# Update flake inputs
nix flake update /etc/nixos --impure
```

## Architecture

This is a NixOS flake-based dotfiles repo. The entry point is `flake.nix`, which wires together all subsystems.

### Key Design Patterns

**Module auto-discovery**: The `lib/modules.nix` helpers (`mapModules`, `mapModulesRec`, `mapModulesRec'`) automatically discover and import `.nix` files from a directory. Files/dirs prefixed with `_` are excluded. `default.nix` in a directory acts as the directory's module; other `.nix` files become named entries.

**Custom lib extensions**: `lib.my` is a custom extension to nixpkgs' lib, defined in `lib/`. It provides:
- `mapModules`/`mapHosts` — directory scanning helpers
- `mkOpt`/`mkOpt'`/`mkBoolOpt` — option shorthand (`lib/options.nix`)
- Path constants: `dotFilesDir`, `modulesDir`, `configDir`, `binDir`, `username`, `homeDir` (`lib/paths.nix`)

**Home Manager aliases**: Rather than the verbose `home-manager.users.jake.home.file`, this config exposes three shorter aliases (defined in `modules/options.nix`):
- `home.file` → `home-manager.users.jake.home.file`
- `home.configFile` → `home-manager.users.jake.xdg.configFile`
- `home.dataFile` → `home-manager.users.jake.xdg.dataFile`

**`user` option**: A top-level `user` option attr set is aliased to `users.users.jake`, letting modules set user properties without hardcoding the username.

**`env` option**: A top-level `env` attrset is injected via `environment.extraInit` as shell exports.

### Directory Structure

- `flake.nix` — defines all inputs and wires together overlays, packages, modules, and hosts
- `default.nix` — root NixOS module: imports home-manager, configures nix settings, boot, fonts
- `lib/` — custom library functions loaded as `lib.my`
- `modules/` — NixOS/home-manager modules, auto-imported recursively; organized by category:
  - `desktop/` — window managers (sway, wayfire, gnome)
  - `dev/` — language toolchains (rust, node, python, etc.)
  - `shell/` — shell config (fish, nushell, direnv, tmux, etc.)
  - `editors/` — editor config
  - `hardware/`, `services/`, `media/`, `themes/`, `wayland/`, etc.
  - `options.nix` — defines global custom options (`user`, `home.*`, `env`)
  - `security.nix`, `xdg.nix` — global NixOS settings
- `hosts/` — per-machine configurations; each becomes a `nixosConfiguration`
  - `personal.nix` — shared config imported by personal machines (timezone, locale, user groups)
  - `xps/`, `work/`, `vultr/`, `iso-install/` — individual host configs
- `overlays/` — nixpkgs overlays, auto-discovered
- `packages/` — custom packages callable via `pkgs.my.<name>`
- `config/` — dotfiles/config files deployed via `home.file` or `home.configFile`
- `bin/` — scripts, added to PATH via `env.PATH`
- `keys/` — SSH/GPG public keys
- `templates/` — flake templates

### Dual nixpkgs Inputs

Two nixpkgs channels are tracked: `nixpkgs` (nixos-24.05, stable) and `nixpkgs-unstable` (master). Unstable packages are accessible as `pkgs.unstable.<name>` within any module.

### Host Configuration Pattern

Each host in `hosts/<name>/default.nix` imports `../personal.nix` plus hardware config, then enables desired modules via `modules.<category>.<name>.enable = true`.
