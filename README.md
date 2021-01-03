[![Made with Doom Emacs](https://img.shields.io/badge/Made_with-Doom_Emacs-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)](https://github.com/hlissner/doom-emacs)
[![NixOS 20.09](https://img.shields.io/badge/NixOS-v20.09-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)

A fork of [hlissner's dotfiles](https://github.com/hlissner/dotfiles) with substantial modifications - most notably the addition of Wayland and the Nord theme - for my own usage.

<img src="/../screenshots/nordic/sway-1.jpg" width="100%" />
<img src="/../screenshots/nordic/sway-2.jpg" width="100%" />

Feel free to poke around and contact me if you have any questions : )



## Installation
Follow the default NixOS install instructions; formatting the disk and what
have you. These steps are detailed in the NixOS install instructions.

I prefer to `chown /etc/nixos` so that you can control it without being a
root user, but this is down to personal preference.

Now that you have a `configuration.nix`, enable flakes on your system (they
aren't available by default as of 20.09). Add the following to your
`configuration.nix` and rebuild:
#+begin_src nix
{ pkgs, ... }: {
   nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
   };
}
#+end_src
If that built successfully, you should now have flakes available on your system.

Clone this repository.
Make a new folder in `hosts/` with the name of your system (change the name
to something other than `nixos`, preferably!) and put your
`configuration.nix` and `hardware-configuration.nix` in it. 
Rename `hosts/${hostname}/configuration.nix` to `default.nix`.

In the new `default.nix`, copy the structure of one of the other
`default.nix` files to enable the modules that you'd like to run.

You should now be able to rebuild. If there are duplicate configuration
options, preferentially remove those from your configuration, but not without
making sure that you don't remove something important!

You now have a working system with this configuration : )
