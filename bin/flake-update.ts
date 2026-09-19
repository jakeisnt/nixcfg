#!/usr/bin/env bun
import { $ } from "bun";
import { existsSync, mkdirSync } from "node:fs";
import { homedir } from "node:os";
import { resolve } from "node:path";
import { parseArgs } from "node:util";

async function main() {
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
}

if (import.meta.main && process.env.NODE_ENV !== "test") await main();

// Run with: bun test ./bin/flake-update.ts
if (process.env.NODE_ENV === "test") {
  const { test, expect } = await import("bun:test");
  const { mkdtempSync, rmSync, chmodSync } = await import("node:fs");
  const { tmpdir } = await import("node:os");

  for (const mode of ["success", "unchanged", "failure", "unexpected"]) {
    test(`update: ${mode}`, async () => {
      const root = mkdtempSync(`${tmpdir()}/flake-update-`);
      const repo = `${root}/repo with spaces`;
      const remote = `${root}/remote`;
      const state = `${root}/state with spaces`;
      const tools = `${root}/bin`;
      // Real Git repositories; Nix and desktop notifications stay local stubs.
      const env = { ...process.env, PATH: `${tools}:${process.env.PATH}`,
        GIT_CONFIG_GLOBAL: "/dev/null", GIT_CONFIG_NOSYSTEM: "1",
        GIT_AUTHOR_NAME: "Test", GIT_AUTHOR_EMAIL: "test@example.com",
        GIT_COMMITTER_NAME: "Test", GIT_COMMITTER_EMAIL: "test@example.com",
        NODE_ENV: "production", STATE_DIRECTORY: state, MODE: mode, BUILD_LOG: `${root}/builds` };
      try {
        mkdirSync(tools);
        await $`git init --bare ${remote}`.env(env).quiet();
        await $`git init -b main ${repo}`.env(env).quiet();
        await Bun.write(`${repo}/flake.lock`, "old\n");
        await $`git -C ${repo} add flake.lock`.env(env).quiet();
        await $`git -C ${repo} commit -m initial`.env(env).quiet();
        await $`git -C ${repo} remote add origin ${remote}`.env(env).quiet();
        await $`git -C ${repo} push origin main`.env(env).quiet();
        await Bun.write(`${repo}/local-edit`, "preserve me");
        await Bun.write(`${tools}/nix`, `#!${process.execPath}
const [command, arg] = Bun.argv.slice(2);
const mode = process.env.MODE;
if (command === "flake" && arg === "update") {
  if (mode !== "unchanged") await Bun.write("flake.lock", "new\\n");
} else if (command === "flake" && arg === "check") {
  if (mode === "failure") process.exit(42);
} else if (command === "eval") {
  console.log(JSON.stringify(["work", "xps"]));
} else if (command === "build") {
  const { appendFileSync } = await import("node:fs");
  appendFileSync(process.env.BUILD_LOG, arg + "\\n");
  if (mode === "unexpected") await Bun.write("surprise", "unexpected");
} else process.exit(99);
`);
        await Bun.write(`${tools}/notify-send`, `#!${process.execPath}\nprocess.exit(0);\n`);
        chmodSync(`${tools}/nix`, 0o755);
        chmodSync(`${tools}/notify-send`, 0o755);
        const invoke = () => $`${process.execPath} ${import.meta.path} ${repo} main`.env(env).quiet().nothrow();
        const result = await invoke();
        expect(result.exitCode).toBe(mode === "success" || mode === "unchanged" ? 0 : 1);
        expect(await Bun.file(`${repo}/flake.lock`).text()).toBe("old\n");
        expect(await Bun.file(`${repo}/local-edit`).text()).toBe("preserve me");
        const refs = await $`git --git-dir ${remote} for-each-ref --format="%(refname)"`.env(env).text();
        expect(refs.includes("refs/heads/updates/")).toBe(mode === "success");
        expect(existsSync(`${state}/worktree`)).toBe(mode === "failure" || mode === "unexpected");
        if (mode === "success") {
          expect(await Bun.file(env.BUILD_LOG).text()).toBe(".#checks.x86_64-linux.work\n.#checks.x86_64-linux.xps\n");
        }
        if (mode === "failure") {
          const retry = await invoke();
          expect(retry.exitCode).toBe(1);
          expect(retry.stdout.toString()).toContain("Previous worktree exists");
        }
      } finally {
        rmSync(root, { recursive: true, force: true });
      }
    });
  }
}
