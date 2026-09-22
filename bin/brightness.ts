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
  await $`brightnessctl --class=backlight --min-value=0 set ${`${Math.max(0, Math.min(100, percent))}%`}`;
}
function draw(percent: number): void {
  process.stdout.write(`\x1b[2J\x1b[H\x1b[?25l  ☼  Display brightness\n\n    ${bar(percent)}  ${String(percent).padStart(3)}%\n\n    ↑/↓ adjust · Esc quit\n`);
}
async function main(): Promise<void> {
  if (!process.stdin.isTTY || !process.stdout.isTTY) throw new Error("brightness needs to run in a terminal (TTY).");
  let percent = await brightness();
  draw(percent);
  process.stdin.setRawMode(true);
  process.stdin.resume();
  process.on("exit", () => { process.stdin.setRawMode(false); process.stdin.pause(); process.stdout.write("\x1b[?25h\x1b[2J\x1b[H"); });
  process.on("SIGINT", () => process.exit(0));
  for await (const chunk of process.stdin) {
    const key = chunk.toString();
    if (key.includes("\x1b") && !key.startsWith("\x1b[")) break;
    if (key === "\x1b[A") { percent = Math.min(100, percent + STEP); await setBrightness(percent); draw(percent); }
    else if (key === "\x1b[B") { percent = Math.max(0, percent - STEP); await setBrightness(percent); draw(percent); }
  }
}
if (import.meta.main && process.env.NODE_ENV !== "test") main().catch((error: unknown) => { process.stdout.write("\x1b[?25h"); console.error(`brightness: ${error instanceof Error ? error.message : error}`); process.exitCode = 1; });
if (process.env.NODE_ENV === "test") {
  const { expect, test } = await import("bun:test");
  test("renders a clamped brightness bar", () => { expect(bar(0, 4)).toBe("[░░░░]"); expect(bar(50, 4)).toBe("[██░░]"); expect(bar(100, 4)).toBe("[████]"); });
}
