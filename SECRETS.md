# Secret storage migration plan

Status: proposed; secret storage has not been migrated.

## Recommendation and security boundary

Use SOPS with dedicated age identities and sops-nix. Keep encrypted files in
Git and decrypt on the target host during activation. Nix expressions should
refer to runtime paths, never plaintext secret values. A decrypted tracked file
in a flake checkout can enter the Nix store through the source snapshot, even
without explicit interpolation into generated configuration.

This separates secrets from evaluation and builds. It does not protect runtime
plaintext from root or the consuming account. Desktop applications running as
the same user share that user's access; stronger isolation requires separate
service identities or on-demand credential access.

[sops-nix](https://github.com/Mic92/sops-nix) supports NixOS, nix-darwin,
activation-time decryption, explicit permissions, and runtime templates.
[Agenix](https://github.com/ryantm/agenix) is a sound alternative for individually
encrypted files; SOPS is preferred here for structured credentials and templates.

## Repository findings

- `lib/paths.nix` imports a root-level `secrets.nix`, while the tracked file is
  `lib/secrets.nix`. Resolve this inconsistency by retiring the legacy import.
- `bin/darwin-bootstrap.ts` unlocks git-crypt before initial evaluation.
- At inspection, neither secret path had an active Git encryption filter.
  Secret contents were not decrypted or displayed.
- No host enables `modules.messengers.email`. Its unfinished OfflineIMAP setup
  and unused SOPS references have been removed. Mutt and msmtp still contain
  legacy `secrets.email.*` references; revisit them only if the module is needed.
- ACME refers to an email address through `secrets.gmail.user`; determine whether
  this belongs in ordinary configuration instead.

## Migration steps

1. **Inventory consumers.** Record each credential's purpose, required hosts,
   consuming account or service, and rotation procedure without recording values.
   Separate public configuration from credentials and drop unused consumers.

2. **Establish identities and recovery.** Generate a dedicated age identity for
   each host, outside the checkout with root-only permissions. Use separate
   administrator and offline recovery identities. Commit only public recipients.
   Test recovery before deployment. Replacement hosts can receive new identities
   and have the necessary files re-encrypted to them.

3. **Partition access.** Add `.sops.yaml` and encrypted files such as
   `secrets/work.yaml`, `secrets/xps.yaml`, and narrowly scoped shared files.
   Grant each file only its required host, administrator, and recovery recipients.
   Every recipient can decrypt the entire file: selecting fields in a module
   does not provide cryptographic isolation.

4. **Integrate runtime delivery.** Pin sops-nix in the flake and import the
   appropriate platform modules. Declare owners and restrictive permissions.
   Prefer service credential/file interfaces; use
   [runtime templates](https://github.com/Mic92/sops-nix#templates) when an
   application requires embedded values. Configure service ordering and restart
   behavior for changed credentials. Remove `lib.my.secrets` and plaintext
   interpolation. Never read decrypted paths with `builtins.readFile`, generate
   plaintext store files, or export credentials through global shell settings.

5. **Separate bootstrap from provisioning.** Remove git-crypt unlocking from the
   Darwin bootstrap. Fresh checkouts must evaluate and build without private
   keys. Provision host identities separately before activating dependent
   services. Keep production identities and secrets out of the installer.
   Verify macOS runtime storage, permissions, and lifecycle separately from Linux.

6. **Validate with dummy credentials first.** Evaluate all supported targets and
   build native targets without decryption keys. Check output closures for dummy
   plaintext. Verify intended-account access, denial to unrelated accounts,
   dependent-service failure when keys are unavailable, and successful reboot,
   rotation, and recovery on both platforms. Use `hey check` and
   `nix flake check --all-systems --no-build` as repository checks during migration.

7. **Retire and rotate.** After consumers migrate, remove legacy secret files
   and git-crypt integration. Rotate credentials that may have entered store
   paths, caches, or backups, then address retained copies. Removing a recipient
   cannot revoke access to historical ciphertext; rotate the actual credential.
   Coordinate rollback with rotation so old generations do not restore revoked
   credentials.

## Completion criteria

- A fresh checkout evaluates and builds without private identities or unlocking.
- Only ciphertext, public recipients, and runtime paths enter the Nix store.
- Hosts can decrypt only their assigned files; runtime access is explicit.
- Bootstrap, recovery, rotation, and credential-aware rollback are documented
  and exercised.
- Legacy plaintext imports and git-crypt dependencies are removed.
