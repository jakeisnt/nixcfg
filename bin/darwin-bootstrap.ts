#!/usr/bin/env bun
import { $, type ShellOutput } from "bun";
import { existsSync, mkdirSync, mkdtempSync, renameSync, rmSync } from "node:fs";
import { homedir, userInfo } from "node:os";
import { dirname, join, resolve } from "node:path";
import { parseArgs } from "node:util";

const REPO = process.env.DOTFILES_REPO || "https://github.com/jakeisnt/nixcfg.git";
const TARGET = process.env.DARWIN_TARGET || "mac";
const DARWIN_USER = process.env.DARWIN_USER || userInfo().username;

function expandPath(path: string): string {
  return resolve(path.replace(/^~(?=\/|$)/, homedir()));
}

export function flakePath(): string {
  if (process.env.DOTFILES) return expandPath(process.env.DOTFILES);
  if (existsSync(resolve(import.meta.dir, "..", "flake.nix"))) return resolve(import.meta.dir, "..");
  return expandPath(process.env.DOTFILES_DIR || "~/.config/nixcfg");
}

function usage(): string {
  return `Usage: darwin-bootstrap [--repair-nix [--yes]]

  --repair-nix  uninstall and reinstall Nix when its nixbld group has the
                wrong GID for nix-darwin
  --yes         confirm a destructive Nix reinstall (required without a TTY)
`;
}

function fail(message: string, code = 1): never {
  console.error(`error: ${message}`);
  process.exit(code);
}

async function run(command: string[]): Promise<ShellOutput> {
  const [program, ...args] = command;
  return $`${program} ${args}`.nothrow();
}

async function commandExists(command: string): Promise<boolean> {
  return (await run(["sh", "-c", `command -v "$1" >/dev/null 2>&1`, "sh", command])).exitCode === 0;
}

async function installNix(): Promise<void> {
  console.log("Installing Nix with the Determinate Nix Installer.");
  let installer: ArrayBuffer;
  try {
    const response = await fetch("https://install.determinate.systems/nix");
    if (!response.ok) throw new Error(`${response.status} ${response.statusText}`);
    installer = await response.arrayBuffer();
  } catch (error) {
    fail(`could not download the Nix installer: ${error}`);
  }
  const result = Bun.spawn(["sh", "-s", "--", "install", "--no-confirm"], { stdin: installer });
  if ((await result.exited) !== 0) fail("the Nix installer failed");

  const profile = "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh";
  if (existsSync(profile)) {
    const path = await $`sh -c ${`. ${profile}; printf '%s' "$PATH"`}`.text();
    process.env.PATH = path;
  }
  if (!(await commandExists("nix"))) {
    fail("Nix installed, but it is not available in this shell; open a new terminal and retry");
  }
}

async function nixbldGid(): Promise<string> {
  const result = await run(["dscl", ".", "-read", "/Groups/nixbld", "PrimaryGroupID"]);
  for (const line of result.stdout.toString().split("\n")) {
    const fields = line.trim().split(/\s+/);
    if (fields.length >= 2 && fields[0] === "PrimaryGroupID:") return fields[1];
  }
  return "";
}

async function repairNixInstallation(assumeYes: boolean): Promise<void> {
  const installer = "/nix/nix-installer";
  if (!existsSync(installer)) {
    fail("/nix/nix-installer is missing; cannot safely perform the deterministic reinstall\nInstall Nix with the Determinate Nix Installer, then rerun with --repair-nix.");
  }
  if (!assumeYes) {
    if (!process.stdin.isTTY || !process.stdout.isTTY) {
      fail(`refusing to uninstall Nix without a TTY\nRerun with: ${process.argv[1]} --repair-nix --yes`);
    }
    const answer = prompt("Nix will be uninstalled and reinstalled. Continue? [y/N] ") || "";
    if (!["y", "yes"].includes(answer.toLowerCase())) {
      console.log("Nix reinstall cancelled.");
      process.exit(1);
    }
  }

  console.log("Uninstalling Nix to repair the nixbld group ID.");
  const uninstall = ["sudo", installer, "uninstall", "--no-confirm"];
  if ((await run(uninstall)).exitCode !== 0) {
    console.error("Nix uninstall could not remove /etc/nix/nix.conf; clearing the stale file and retrying.");
    if ((await run(["sudo", "/usr/bin/chflags", "nouchg", "/etc/nix/nix.conf"])).exitCode !== 0) {
      console.error("warning: could not clear the file flags on /etc/nix/nix.conf");
    }
    if ((await run(["sudo", "/bin/rm", "-f", "/etc/nix/nix.conf"])).exitCode !== 0) {
      fail("could not remove /etc/nix/nix.conf; refusing to continue");
    }
    if ((await run(uninstall)).exitCode !== 0) fail("Nix uninstall failed even after removing /etc/nix/nix.conf");
  }
  await installNix();
  const gid = await nixbldGid();
  if (gid !== "350") fail(`Nix was reinstalled, but nixbld still has an unexpected GID: ${gid}`);
}

async function downloadFlake(flake: string): Promise<void> {
  if (existsSync(join(flake, "flake.nix"))) return;
  if (existsSync(flake)) fail(`${flake} exists but is not a Nix flake checkout`);
  console.log(`Downloading ${REPO} into ${flake}`);
  mkdirSync(dirname(flake), { recursive: true });
  const temp = mkdtempSync(join(process.env.TMPDIR || "/tmp", "nixcfg-"));
  try {
    const archive = join(temp, "nixcfg.tar.gz");
    const response = await fetch(`${REPO.replace(/\.git$/, "")}/archive/refs/heads/main.tar.gz`);
    if (!response.ok) throw new Error(`${response.status} ${response.statusText}`);
    await Bun.write(archive, response);
    const unpacked = join(temp, "unpacked");
    mkdirSync(unpacked);
    const listing = (await $`tar -tzf ${archive}`.text()).split("\n").filter(Boolean);
    if (listing.some((path) => path.startsWith("/") || path.split("/").includes(".."))) {
      fail("downloaded archive contains an unsafe path");
    }
    await $`tar -xzf ${archive} -C ${unpacked}`;
    const sourceDirs = (await Array.fromAsync(new Bun.Glob("*/").scan({ cwd: unpacked, onlyFiles: false }))).map((path) => path.replace(/\/$/, ""));
    if (sourceDirs.length !== 1 || !existsSync(join(unpacked, sourceDirs[0], "flake.nix"))) {
      fail("downloaded archive does not contain a flake");
    }
    renameSync(join(unpacked, sourceDirs[0]), flake);
  } catch (error) {
    if (error instanceof Error && error.message.startsWith("error:")) throw error;
    fail(`could not download the configuration: ${error}`);
  } finally {
    rmSync(temp, { recursive: true, force: true });
  }
}

export async function main(args = Bun.argv.slice(2)): Promise<void> {
  const { values } = parseArgs({
    args,
    options: {
      "repair-nix": { type: "boolean", default: false },
      yes: { type: "boolean", default: false },
      help: { type: "boolean", short: "h", default: false },
    },
    strict: true,
  });
  if (values.help) {
    console.log(usage());
    return;
  }
  const repair = values["repair-nix"] ?? false;
  const assumeYes = values.yes ?? false;
  if (process.platform !== "darwin") fail("darwin-bootstrap must run on macOS");
  if (process.arch !== "arm64") fail("this configuration currently supports Apple Silicon (arm64) only");
  if (process.getuid?.() === 0 || !DARWIN_USER || DARWIN_USER === "root") fail("run this as your normal user; the script invokes sudo when needed");

  const flake = flakePath();
  await downloadFlake(flake);
  if (!(await commandExists("nix"))) await installNix();
  const actualGid = await nixbldGid();
  if (actualGid && actualGid !== "350") {
    if (repair) await repairNixInstallation(assumeYes);
    else fail(`nixbld has GID ${actualGid}, but nix-darwin expects GID 350\nRerun with: ${process.argv[1]} --repair-nix`);
  }

  process.chdir(flake);
  let result: ShellOutput;
  if (await commandExists("darwin-rebuild")) {
    result = await run(["sudo", "darwin-rebuild", "switch", "--flake", `${flake}#${TARGET}`, "--option", "pure-eval", "no"]);
  } else {
    console.log("darwin-rebuild is not installed; evaluating and activating nix-darwin once.");
    result = await run(["sudo", "nix", "--extra-experimental-features", "nix-command flakes", "run", "nix-darwin", "--", "switch", "--flake", `${flake}#${TARGET}`, "--option", "pure-eval", "no"]);
  }
  if (result.exitCode !== 0) process.exitCode = result.exitCode;
}

if (import.meta.main && process.env.NODE_ENV !== "test") {
  main().catch((error: unknown) => {
    console.error(`darwin-bootstrap: ${error instanceof Error ? error.message : error}`);
    process.exitCode = 1;
  });
}

if (process.env.NODE_ENV === "test") {
  const { expect, test } = await import("bun:test");
  test("uses the checkout flake when available", () => {
    expect(flakePath()).toBe(resolve(import.meta.dir, ".."));
  });
  test("prints help without touching the system", async () => {
    const output: string[] = [];
    const originalLog = console.log;
    console.log = (message: string) => output.push(message);
    try { await main(["--help"]); } finally { console.log = originalLog; }
    expect(output.join("\n")).toContain("Usage: darwin-bootstrap");
  });
}
