#!/usr/bin/env bun
import { $ } from "bun";
import { existsSync, mkdirSync } from "node:fs";
import { homedir } from "node:os";
import { resolve } from "node:path";
import { parseArgs } from "node:util";

const { values, positionals } = parseArgs({
  args: Bun.argv.slice(2),
  options: { locked: { type: "boolean", default: false } },
  allowPositionals: true,
  strict: true,
});
if (positionals.length > 2) throw new Error("Usage: flake-update [repository] [base-branch]");
const repo = resolve(positionals[0] ?? "/etc/nixos");
const base = positionals[1] ?? "main";
const state = resolve(process.env.STATE_DIRECTORY ||
  `${process.env.XDG_STATE_HOME || `${homedir()}/.local/state`}/flake-update`);
const worktree = `${state}/worktree`;
mkdirSync(state, { recursive: true });

// flock owns the lock for the entire child lifetime and releases it on failure.
if (!values.locked) {
  const result = await $`flock --nonblock --conflict-exit-code 75 ${`${state}/lock`} ${process.execPath} ${import.meta.path} --locked -- ${repo} ${base}`.nothrow();
  process.exit(result.exitCode === 75 ? 0 : result.exitCode);
}

$.env({ ...process.env, GIT_TERMINAL_PROMPT: "0",
  GIT_SSH_COMMAND: "ssh -o BatchMode=yes -o ConnectTimeout=30" });

async function notify(message: string) {
  console.log(message);
  await $`notify-send 'NixOS update' ${message}`.nothrow();
}

async function update() {
  // Preserve failed attempts for inspection instead of overwriting their work.
  if (existsSync(worktree)) {
    throw new Error(`Previous worktree exists: ${worktree}. Inspect and remove it with git worktree remove before retrying.`);
  }
  await $`git fetch origin ${base}`.cwd(repo);
  const revision = (await $`git rev-parse FETCH_HEAD`.cwd(repo).text()).trim();
  const branch = `updates/flake-${new Date().toISOString().replace(/[-:]/g, "").replace(/\.\d{3}Z$/, "Z")}`;
  await $`git worktree add -b ${branch} ${worktree} ${revision}`.cwd(repo);
  await $`nix flake update --flake . --impure`.cwd(worktree);
  if (!(await $`git status --porcelain --untracked-files=all`.cwd(worktree).text())) {
    await $`git worktree remove ${worktree}`.cwd(repo);
    await notify("No flake updates available.");
    return;
  }

  await $`nix flake check --all-systems --no-build`.cwd(worktree);
  const targets: unknown = await $`nix eval --json '.#checks.x86_64-linux' --apply builtins.attrNames`.cwd(worktree).json();
  if (!Array.isArray(targets) || !targets.length ||
      !targets.every((target) => typeof target === "string" && target.length > 0)) {
    throw new Error("No valid Linux checks discovered.");
  }
  for (const target of targets) {
    await $`nix build ${`.#checks.x86_64-linux.${target}`} --no-link --option pure-eval no`.cwd(worktree);
  }
  await $`git add flake.lock`.cwd(worktree);
  await $`git diff --cached --check`.cwd(worktree);
  await $`git diff --cached --stat`.cwd(worktree);
  // Include untracked files and changes made by build hooks in this guard.
  if (await $`git status --porcelain --untracked-files=all`.cwd(worktree).text() !== "M  flake.lock\n") {
    throw new Error("Unexpected changes in update worktree; refusing to commit.");
  }
  await $`git -c commit.gpgsign=false commit -m 'chore: update flake inputs'`.cwd(worktree);
  await $`git push --atomic origin ${`HEAD:refs/heads/${branch}`}`.cwd(worktree);
  await $`git worktree remove ${worktree}`.cwd(repo);
  await notify(`Verified update pushed to ${branch}. Review and merge, then run hey rebuild.`);
}

try {
  await update();
} catch (error) {
  await notify(`Update failed: ${error}. See journalctl -u flake-update and ${worktree}.`);
  process.exitCode = 1;
}
