[![Made with Doom Emacs](https://img.shields.io/badge/Made_with-Doom_Emacs-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)](https://github.com/hlissner/doom-emacs)
[![NixOS 26.05](https://img.shields.io/badge/NixOS-v26.05-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)

Started as a fork of [hlissner's dotfiles](https://github.com/hlissner/dotfiles), but different design decisions have been made to differentiate the two since.

## Supported systems

This flake actively supports these machines and build targets:

- `work` — Framework laptop running NixOS
- `xps` — Dell XPS 9370 running NixOS
- `mac` — Apple Silicon Mac running nix-darwin

The shared modules in `modules/` are maintained for these hosts. Hosts opt into
the desktop, shell, development, media, hardware, and service modules they need;
the active combinations are documented in [`hosts/README.md`](hosts/README.md).

The supported configurations can be audited with:

``` sh
hey check
```

This evaluates every host and builds the targets supported by the current OS.

### macOS

The Darwin target currently supports Apple Silicon and uses `mac` regardless of
the Mac's computer name. On a fresh checkout, install Nix, then run:

``` sh
curl -fsSL https://raw.githubusercontent.com/jakeisnt/nixcfg/main/bin/darwin-bootstrap | sh
```

That command installs Nix if necessary, downloads the configuration into
`~/.config/nixcfg`, and activates it. Run the same command again to re-activate
from that checkout. If an existing Nix installation has a mismatched `nixbld`
group ID, repair it deterministically with:

``` sh
./bin/darwin-bootstrap --repair-nix
```

The repair asks for confirmation before uninstalling Nix. Automation must use
`--repair-nix --yes` explicitly.

The Darwin user is detected from the account running the command, so the
configuration does not require the account to be named `jake`.

Set `DOTFILES` when the checkout is not the script's parent directory, or set
`DARWIN_TARGET` when adding another Darwin host. After activation, use
`hey build`, `hey rebuild`, and `hey check`. Existing Homebrew packages and
services are intentionally left untouched until their Nix replacements have
been verified.

## Installation

For a configuration-only audit, use `nix flake check --all-systems --no-build`.

It's often the case that older version of Linux don't have support for utilities you want,
and it's nice to have access to a graphical installer for most of the process - which none of the nightly NixOS ISOs support.

Move the installer ISO to a flash drive (`mv path/to/firmware.iso drive-address`) and make sure to `sync` afterwards.

After following the default NixOS install instructions off of that flash drive:

1. Enter a shell with the necessary dependencies.

``` sh
nix-shell -p git nixFlakes
```

2. Clone this repository into the configuration folder.

``` sh
chown -R nixos /mnt/boot/nixos
git clone https://github.com/jakeisnt/nixcfg /mnt/boot/nixos
```
3. Generate a configuration for this machine (ensure that you've mounted swap space)

``` sh
nixos-generate-config --root /mnt
mv configuration.nix hosts/$HOSTNAME/default.nix
mv hardware-configuration.nix hosts/$HOSTNAME/
```

4. Reference previous configurations when rewriting `default.nix` to use the desired format.
Do not mess this up; make sure you give yourself things like a window manager and internet access. remember to import `../personal.nix` from `default.nix` in addition to the hardware configuration.

5. Install the configuration.
``` sh

nixos-install --root /mnt --impure --flake .#$HOSTNAME
```

You should be set! Reboot into the machine you've just configured.
Make sure to commit to this repository with that machine's configuration.
