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

It's often the case that older version of Linux don't have support for utilities you want,
and it's nice to have access to a graphical installer for most of the process - which none of the nightly NixOS ISOs support.

Move that ISO to a flash drive (`mv path/to/firmware.iso drive-address`) and make sure to `sync` afterwards.

After following the default NixOS install instructions off of that flash drive:

``` sh
# acquire necessary dependencies
nix-shell -p git nixFlakes

# clone this repository into the configuration (may have to chown)
git clone https://github.com/jakeisnt/nix-cfg /mnt/boot/nixos

# generate config for this machine (make sure you've mounted swap space so that it's autodetected)
nixos-generate-config --root /mnt
mv configuration.nix hosts/$HOSTNAME/default.nix
mv hardware-configuration.nix hosts/$HOSTNAME/

# reference previous configurations when rewriting `default.nix` to use the desired format.
# do not mess this up; make sure you give yourself things like a window manager and internet access

# install the configuration:
nixos-install --root /mnt --impure --flake .#$HOSTNAMAE
```

You should be set! Reboot into the machine you've just configured. 
Make sure to commit back to this repository with that machine's configuration.
