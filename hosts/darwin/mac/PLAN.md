# macOS nix-darwin plan

Status: initial implementation complete; activation on a real Apple Silicon Mac remains.

## Goal

Make this repository easy to install and maintain on macOS using nix-darwin,
with Nix replacing Homebrew for development tools and supported applications.

## Existing foundation

The flake already exposes an Apple Silicon `darwinConfigurations.mac` host,
with Home Manager, Fish, Git, Node, Rustup, and a few CLI utilities. The current
bootstrap requires Bun and moves system configuration files before building.
The `hey` helper needs review for macOS detection, sudo, checkout location,
and selecting the configured target independently of the current hostname.

## Proposed work

1. Confirm the Mac architecture, existing account's short username, checkout
   location, and Nix installation. Keep Apple Silicon as the initial target;
   add Intel support only if needed. Make machine-specific settings easy to edit.
2. Audit the Darwin module wiring, primary user, Home Manager packages, shell
   environment, XDG paths, and Nix management compatibility. Keep state versions
   stable and avoid importing Linux-only settings.
3. Replace the Bun bootstrap with a script runnable by macOS's bundled shell.
   Validate prerequisites and username, build the locked flake, then activate
   with sudo. Report file conflicts for deliberate backup instead of moving
   system files automatically. No Homebrew dependency for bootstrapping.
4. Fix `hey build`, `hey rebuild`, and `hey check` for macOS. Ensure Nushell is
   installed to run `hey`, and make the checkout and target selection reliable.
5. Inventory the Mac's Homebrew formulae, casks, and services. Add Nix equivalents
   for tools actually used. Likely additions include Bun, Python/uv, Go, build
   tools, Neovim, tmux, and direnv. Prefer project dev shells for project-specific
   dependencies. Document direct vendor/App Store installs for unsupported apps.
6. Document installation, customization, rebuilding, updates, rollback, and
   migration. Disable Homebrew integration; retain existing Homebrew packages
   and service data until their replacements have been verified on the Mac.

## Verification and completion

- Evaluate the Darwin system and all flake targets; inspect platform support
  for the selected packages and check shell-script error paths.
- Build and activate on an actual Mac. Verify login shells, `hey`, development
  tools, direnv, and application availability without Homebrew in PATH.
- Verify the user's real projects and services before separately removing
  Homebrew. Linux evaluation alone cannot validate macOS activation.
- Review the final diff and commit/push the coherent, verified implementation
  according to repository policy after implementation is authorized.

Homebrew removal remains deliberately out of scope until the configuration has
been activated and the user's real projects and services have been verified.
