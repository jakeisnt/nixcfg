#!/usr/bin/env bun
import { $ } from "bun";

const STEP = 5;
export function bar(percent: number, width = 24): string {
  const filled = Math.round((Math.max(0, Math.min(100, percent)) / 100) * width);
  return `[${"█".repeat(filled)}${"░".repeat(width - filled)}]`;
}
async function brightness(): Promise<number> {
  const current = Number((await $`brightnessctl --class=backlight --min-value=0 get`.text()).trim());
  const maximum = Number((await $`brightnessctl --class=backlight max`.text()).trim());
  if (!Number.isFinite(current) || !Number.isFinite(maximum) || maximum <= 0) throw new Error("Could not read the display brightness.");
  return Math.round((current / maximum) * 100);
}
async function setBrightness(percent: number): Promise<void> {
  await $`brightnessctl --class=backlight --min-value=0 set ${`${Math.max(0, Math.min(100, percent))}%`}`.quiet();
}
function draw(percent: number): void {
  const line = `☼ ${String(percent).padStart(3)}% ${bar(percent)}  ↑/↓ adjust · Enter keep · Esc revert`;
  process.stdout.write(`\r\x1b[2K${line.slice(0, Math.max(0, (process.stdout.columns || 80) - 1))}`);
}
async function main(): Promise<void> {
  if (!process.stdin.isTTY || !process.stdout.isTTY) throw new Error("brightness needs to run in a terminal (TTY).");
  const initialPercent = await brightness();
  let percent = initialPercent;
  process.stdin.setRawMode(true);
  process.stdin.resume();
  process.stdout.write("\x1b[?25l");
  const cleanup = () => { process.stdin.setRawMode(false); process.stdin.pause(); process.stdout.write("\x1b[?25h\n"); };
  const interrupt = () => process.exit(0);
  const resize = () => draw(percent);
  process.on("exit", cleanup);
  process.on("SIGINT", interrupt);
  process.stdout.on("resize", resize);
  try {
    draw(percent);
    for await (const chunk of process.stdin) {
      const key = chunk.toString();
      if (key.includes("\x03") || key.includes("\x04")) break;
      if (key === "\r" || key === "\n") break;
      if (key === "\x1b") { await setBrightness(initialPercent); percent = initialPercent; break; }
      if (key === "\x1b[A") { percent = Math.min(100, percent + STEP); await setBrightness(percent); draw(percent); }
      else if (key === "\x1b[B") { percent = Math.max(0, percent - STEP); await setBrightness(percent); draw(percent); }
    }
  } finally {
    process.removeListener("exit", cleanup);
    process.removeListener("SIGINT", interrupt);
    process.stdout.removeListener("resize", resize);
    cleanup();
  }
}
if (import.meta.main && process.env.NODE_ENV !== "test") main().catch((error: unknown) => { process.stdout.write("\x1b[?25h"); console.error(`brightness: ${error instanceof Error ? error.message : error}`); process.exitCode = 1; });
if (process.env.NODE_ENV === "test") {
  const { expect, test } = await import("bun:test");
  test("renders a clamped brightness bar", () => { expect(bar(0, 4)).toBe("[░░░░]"); expect(bar(50, 4)).toBe("[██░░]"); expect(bar(100, 4)).toBe("[████]"); });
}
