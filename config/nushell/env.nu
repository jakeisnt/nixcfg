# Specifies how environment variables are:
# - converted from a string to a value on Nushell startup (from_string)
# - converted from a value back to a string when running external commands (to_string)
# Note: The conversions happen *after* config.nu is loaded

# Port all of the environment variables over to nushell
env | each { |it| echo $"let-env ($it.name) = '($it.raw)'" } | str join (char nl)
clear

# gnupghome is only defined in bash? redefine it
let-env GNUPGHOME = $'($env.XDG_CONFIG_HOME)/gnupg'

let-env ENV_CONVERSIONS = {
  "PATH": {
    from_string: { |s| $s | split row (char esep) }
    to_string: { |v| $v | str collect (char esep) }
  }
  "Path": {
    from_string: { |s| $s | split row (char esep) }
    to_string: { |v| $v | str collect (char esep) }
  }
}

# Directories to search for scripts when calling source or use
#   By default, <nushell-config-dir>/scripts is added
let-env NU_LIB_DIRS = [
    ($nu.config-path | path dirname | path join 'scripts')
]

# Directories to search for plugin binaries when calling register
#   By default, <nushell-config-dir>/plugins is added
let-env NU_PLUGIN_DIRS = [
    ($nu.config-path | path dirname | path join 'plugins')
]

# Add /bin to path
let-env PATH = ($env.PATH | split row (char esep) | prepend '/etc/nixos/bin')
let-env PATH = ($env.PATH | split row (char esep) | prepend $'($env.HOME)/.emacs.d/bin')

$env.GDK_BACKEND = wayland;

alias js = joshuto
# TODO: is there a better wifi testing tool?
alias ct = ping google.com

mkdir ~/.cache/starship
starship init nu | save -f ~/.cache/starship/init.nu

