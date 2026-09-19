# AGENTS.md

Guidance for contributors and coding agents working in this NixOS/nix-darwin flake.

## Repository

- `flake.nix` is the entry point. It defines the `work` and `xps` NixOS targets and the `mac` Apple Silicon nix-darwin target.
- `modules/` contains reusable NixOS and Home Manager modules; `hosts/` contains host configurations; `config/` contains deployed dotfiles; `overlays/` and `packages/` contain flake extensions.
- Modules and hosts are auto-discovered by the helpers in `lib/`; do not add imports manually unless the discovery rules require it.
- The default interactive shell is Nushell. `bin/hey` is the project command wrapper.

## Validation and operations

Use the least destructive relevant check:

```sh
nix flake check --all-systems --no-build  # configuration-only audit
hey check                                 # audit plus native target builds
hey build                                 # build the current target
hey rebuild                               # activate the current target
hey upgrade                               # update inputs and rebuild
```

Run commands from the repository checkout. Do not run broad formatters or fixers unless explicitly requested.

## Scripts

All sufficiently complex scripts must be Bun TypeScript CLI programs, not Bash (or another scripting language). Use Bash only for genuinely tiny, straightforward glue programs. Complex CLIs should have explicit argument handling, useful errors, and noninteractive-safe behavior; destructive actions must require an explicit confirmation flag when run without a TTY.

## Git workflow

Keep commits atomic and include only intentional changes. Before committing, inspect `git status --short` and `git diff --cached --stat`, stage only the relevant files, run the relevant validation, then commit and push with `git push --atomic`. Report any validation, commit, or push blocker and its exact follow-up.
