#!/usr/bin/env bun
// darwin-bootstrap.ts — bootstrap or rebuild nix-darwin on macOS
//
// First run: installs nix-darwin (darwin-rebuild not yet available)
// Subsequent runs: calls darwin-rebuild switch directly

import { $ } from "bun";
import { existsSync } from "fs";
import { join } from "path";

const FLAKE = join(import.meta.dir, "..");
const TARGET = "mac";

async function fileExists(path: string): Promise<boolean> {
  return existsSync(path);
}

async function backupIfExists(path: string): Promise<void> {
  const backup = `${path}.backup-before-nix-darwin`;
  if (await fileExists(path) && !(await fileExists(backup))) {
    console.log(`  Backing up ${path} → ${backup}`);
    await $`sudo mv ${path} ${backup}`;
  }
}

async function darwinRebuildExists(): Promise<boolean> {
  try {
    await $`which darwin-rebuild`.quiet();
    return true;
  } catch {
    return false;
  }
}

async function main() {
  console.log(`\nnix-darwin bootstrap for host: ${TARGET}\n`);

  if (await darwinRebuildExists()) {
    console.log("darwin-rebuild found — running switch...\n");
    await $`sudo darwin-rebuild switch --flake ${FLAKE}#${TARGET}`.cwd(FLAKE);
    return;
  }

  console.log("darwin-rebuild not found — running first-time bootstrap.\n");

  // nix-darwin wants to manage these files; back them up so it can create symlinks
  const managed = [
    "/etc/nix/nix.conf",
    "/etc/shells",
    "/etc/bashrc",
    "/etc/zshrc",
  ];

  // Unlock git-crypt encrypted files (e.g. lib/secrets.nix) before nix evaluation
  const secretsPath = join(FLAKE, "lib/secrets.nix");
  if (existsSync(secretsPath)) {
    const contents = await Bun.file(secretsPath).text();
    if (contents.startsWith("\x00GITCRYPT")) {
      console.log("Unlocking git-crypt encrypted files...\n");
      try {
        await $`git-crypt unlock`.cwd(FLAKE);
      } catch {
        console.error("git-crypt unlock failed. Provide a key or unlock manually before bootstrapping.");
        process.exit(1);
      }
    }
  }

  console.log("Backing up files that nix-darwin will manage:");
  for (const f of managed) {
    await backupIfExists(f);
  }

  console.log("\nRunning: nix run nix-darwin -- switch --flake .#" + TARGET + "\n");
  await $`sudo nix --extra-experimental-features "nix-command flakes" run nix-darwin -- switch --flake ${FLAKE}#${TARGET}`.cwd(FLAKE);

  console.log("\nBootstrap complete. You can now use: darwin-rebuild switch --flake .#" + TARGET);
}

main().catch((err) => {
  console.error("Error:", err.message ?? err);
  process.exit(1);
});
