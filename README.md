[![Made with Doom Emacs](https://img.shields.io/badge/Made_with-Doom_Emacs-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)](https://github.com/hlissner/doom-emacs)
[![NixOS 20.09](https://img.shields.io/badge/NixOS-v20.09-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)

Started as a fork of [hlissner's dotfiles](https://github.com/hlissner/dotfiles), but different design decisions have been made to differentiate the two since.

## Screenshots
### Full Configuration
<img src="/../screenshots/nordic/neofetch-desktop.png" width="100%" />
<img src="/../screenshots/nordic/emacs-workspace.png" width="100%" />
<img src="/../screenshots/nordic/spotify-scrot.png" width="100%" />

### Minimal Configuration
<img src="/../screenshots/nordic/sway-1.jpg" width="100%" />
<img src="/../screenshots/nordic/sway-2.jpg" width="100%" />

Feel free to poke around and contact me if you have any questions : )


## Installation

First, snag a copy of the newest version of NixOS by building it off of a previous machine from source.

This configuration offers `usb`, a CLI-based live USB configuration with some nice utilities for getting started.

If you'd like to use that system, load the ISO onto a USB with the following commands from an existing Nix system with Nix Flakes enabled:

``` sh
nix build .#nixosConfigurations.iso-install.config.system.build.isoImage --impure
sudo cp /path/to/iso/in/nix/store /dev/sda-usb-device-name
sudo sync
```

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
