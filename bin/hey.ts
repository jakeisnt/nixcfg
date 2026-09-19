#!/usr/bin/env bun
import { $ } from "bun";
import { homedir } from "node:os";
import { parseArgs } from "node:util";

const LINUX_CHECK_TARGETS = ["work", "xps"] as const;

type Platform = "darwin" | "linux";
type Options = { yes: boolean };

export function platform(): Platform {
  if (process.platform === "darwin") return "darwin";
  if (process.platform === "linux") return "linux";
  throw new Error(`Unsupported platform: ${process.platform}`);
}

export function flakePath(currentPlatform: Platform = platform()): string {
  if (currentPlatform === "darwin") {
    return process.env.DOTFILES || `${process.env.HOME || homedir()}/Documents/jakeisnt/nixcfg`;
  }
  return "/etc/nixos";
}

export function darwinTarget(): string {
  return process.env.DARWIN_TARGET || "mac";
}

function usage(): string {
  return `Usage: hey [--yes] <command> [arguments]

Commands:
  build                  Build the current system without switching
  check                  Evaluate the flake and build all native checks
  find <package>         Search nixpkgs for a package
  gc                     Collect old Nix store generations
  rebuild                Build and switch to the current system
  upgrade                Update flake inputs, then rebuild

Options:
  --yes                  Allow destructive commands in non-interactive use
  -h, --help             Show this help
`;
}

function requireConfirmation(command: string, options: Options): void {
  if (!process.stdin.isTTY && !options.yes) {
    throw new Error(`${command} requires an interactive terminal or --yes`);
  }
}

async function gc(options: Options): Promise<void> {
  requireConfirmation("gc", options);
  await $`nix-collect-garbage -d`;
}

async function rebuild(options: Options): Promise<void> {
  requireConfirmation("rebuild", options);
  const flake = flakePath();
  if (platform() === "darwin") {
    await $`darwin-rebuild switch --flake ${`${flake}#${darwinTarget()}`} --option pure-eval no`;
  } else {
    await $`sudo nixos-rebuild switch --flake ${flake} --option pure-eval no`;
  }
}

async function build(): Promise<void> {
  const flake = flakePath();
  if (platform() === "darwin") {
    await $`nix build ${`${flake}#darwinConfigurations.${darwinTarget()}.system`} --option pure-eval no`;
  } else {
    await $`sudo nixos-rebuild build --flake ${flake} --option pure-eval no`;
  }
}

async function findPackage(packageName: string): Promise<void> {
  await $`nix search nixpkgs ${packageName}`;
}

async function check(): Promise<void> {
  const flake = flakePath();
  await $`nix flake check ${flake} --all-systems --no-build`;

  if (platform() === "darwin") {
    await $`nix build ${`${flake}#checks.aarch64-darwin.${darwinTarget()}`} --option pure-eval no`;
    return;
  }

  for (const target of LINUX_CHECK_TARGETS) {
    await $`nix build ${`${flake}#checks.x86_64-linux.${target}`} --option pure-eval no`;
  }
}

async function upgrade(options: Options): Promise<void> {
  requireConfirmation("upgrade", options);
  const flake = flakePath();
  await $`nix flake update --flake ${flake} --impure`;
  await rebuild(options);
}

export async function main(args = Bun.argv.slice(2)): Promise<void> {
  const { values, positionals } = parseArgs({
    args,
    options: {
      yes: { type: "boolean", short: "y", default: false },
      help: { type: "boolean", short: "h", default: false },
    },
    allowPositionals: true,
    strict: true,
  });
  const options = { yes: values.yes ?? false };
  const [command, ...arguments_] = positionals;

  if (values.help || !command) {
    console.log(usage());
    return;
  }

  switch (command) {
    case "build":
      if (arguments_.length) throw new Error("Usage: hey build");
      await build();
      break;
    case "check":
      if (arguments_.length) throw new Error("Usage: hey check");
      await check();
      break;
    case "find":
      if (arguments_.length !== 1) throw new Error("Usage: hey find <package>");
      await findPackage(arguments_[0]);
      break;
    case "gc":
      if (arguments_.length) throw new Error("Usage: hey gc");
      await gc(options);
      break;
    case "rebuild":
      if (arguments_.length) throw new Error("Usage: hey rebuild");
      await rebuild(options);
      break;
    case "upgrade":
      if (arguments_.length) throw new Error("Usage: hey upgrade");
      await upgrade(options);
      break;
    default:
      throw new Error(`Unknown command: ${command}\n\n${usage()}`);
  }
}

if (import.meta.main && process.env.NODE_ENV !== "test") {
  main().catch((error: unknown) => {
    console.error(`hey: ${error instanceof Error ? error.message : error}`);
    process.exitCode = 1;
  });
}

if (process.env.NODE_ENV === "test") {
  const { expect, test } = await import("bun:test");
  test("resolves platform defaults", () => {
    expect(flakePath("linux")).toBe("/etc/nixos");
    expect(darwinTarget()).toBe(process.env.DARWIN_TARGET || "mac");
  });

  test("prints help without a command", async () => {
    const output: string[] = [];
    const originalLog = console.log;
    console.log = (message: string) => output.push(message);
    try {
      await main([]);
    } finally {
      console.log = originalLog;
    }
    expect(output.join("\n")).toContain("Usage: hey");
  });
}
