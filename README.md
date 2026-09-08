[![Made with Doom Emacs](https://img.shields.io/badge/Made_with-Doom_Emacs-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)](https://github.com/hlissner/doom-emacs)
[![NixOS 26.05](https://img.shields.io/badge/NixOS-v26.05-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)

Started as a fork of [hlissner's dotfiles](https://github.com/hlissner/dotfiles), but different design decisions have been made to differentiate the two since.

## Screenshots
### Full Configuration
<img src="/../screenshots/nordic/neofetch-desktop.png" width="100%" />
<img src="/../screenshots/nordic/emacs-workspace.png" width="100%" />
<img src="/../screenshots/nordic/spotify-scrot.png" width="100%" />

### Minimal Configuration
<img src="/../screenshots/nordic/sway-1.jpg" width="100%" />
<img src="/../screenshots/nordic/sway-2.jpg" width="100%" />

Feel free to poke around. Contact me if you have any questions!

## Supported systems

This flake actively supports these machines and build targets:

- `work` — Framework laptop running NixOS
- `xps` — Dell XPS 9370 running NixOS
- `iso-install` — NixOS installer ISO
- `mac` — Apple Silicon Mac running nix-darwin

The shared modules in `modules/` are maintained for these hosts. Hosts opt into
the desktop, shell, development, media, hardware, and service modules they need;
the active combinations are documented in [`hosts/README.md`](hosts/README.md).

The supported configurations can be audited with:

``` sh
hey check
```

This evaluates every host and builds the targets supported by the current OS.

## Installation

First, snag a copy of the newest version of NixOS by building it off of a previous machine from source.

This configuration offers `usb`, a CLI-based live USB configuration with some nice utilities for getting started.

If you'd like to use that system, load the ISO onto a USB with the following commands from an existing Nix system with Nix Flakes enabled:

``` sh
nix build .#nixosConfigurations.iso-install.config.system.build.isoImage
sudo cp /path/to/iso/in/nix/store /dev/sda-usb-device-name
sudo sync
```

For a configuration-only audit, use `nix flake check --all-systems --no-build`.

It's often the case that older version of Linux don't have support for utilities you want,
and it's nice to have access to a graphical installer for most of the process - which none of the nightly NixOS ISOs support.

Move that ISO to a flash drive (`mv path/to/firmware.iso drive-address`) and make sure to `sync` afterwards.

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
