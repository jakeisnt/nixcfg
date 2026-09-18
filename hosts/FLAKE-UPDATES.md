# Scheduled flake updates

The `work` host prepares updates on Sundays at 09:00, with up to 30 minutes
of jitter. Missed runs are caught up after boot. Enable the timer by rebuilding
after merging this configuration. No configuration is automatically activated.

The service runs as the configured user and fetches `origin/main`. It creates
a separate worktree in `/var/lib/flake-update/worktree`, updates all inputs,
evaluates every flake check and builds every x86_64-linux check (currently
`work` and `xps`). Darwin is evaluated but cannot be built on this Linux host.
Successful updates are committed and pushed atomically to a timestamped
`updates/flake-*` branch. Review and merge that branch, pull it into `/etc/nixos`,
then run `hey rebuild`. Build success does not verify runtime desktop behavior.

The user needs write access to the checkout, Git author configuration, and
noninteractive SSH credentials for origin. The system service does not inherit
an interactive SSH agent; configure credentials usable without an agent or
the job will fail. Commit signing is disabled for these automated lock updates.
No new credentials are installed by this module.

Inspect the schedule with `systemctl list-timers flake-update.timer` and logs
with `journalctl -u flake-update`. Run immediately using
`sudo systemctl start flake-update`. Desktop notifications are best effort and
require the user's session bus and notification daemon; the journal is always
the source of status on a headless host.

Failed worktrees are retained and block subsequent attempts. Inspect their
diff and logs, save anything needed, then remove the worktree as the configured
user with `git -C /etc/nixos worktree remove /var/lib/flake-update/worktree`
(Git requires `--force` if it still has uncommitted changes). Retry the service.
Successful worktrees are removed; update branches remain for review. Each
scheduled run can create a new branch; delete obsolete branches after review.

The module exposes `repository`, `baseBranch`, and `calendar` options under
`modules.services.flake-update`. Enable it on only one machine per repository
to avoid duplicate updates. To run daily, set `calendar = "daily"`.

Run `python3 tests/flake-update.py` with Bash, Git, jq, and flock on PATH to
exercise publication, no-op updates, failed checks, and unexpected changes in
temporary repositories. Nix and notifications are stubbed in these tests.
