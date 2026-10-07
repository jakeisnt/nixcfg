#!/usr/bin/env bun
import { $ } from "bun";
import { parseArgs } from "node:util";

const types = ["_airplay._tcp", "_raop._tcp"];
const help = `Usage: airplay-scan [--interface NAME] [--seconds N] [--json]

Discover AirPlay and RAOP audio services using the local Avahi daemon.
Scans all Avahi interfaces by default, for 12 seconds (maximum 300).
Requires avahi-browse and GNU timeout on PATH; no sudo is needed.

  airplay-scan --interface wlp170s0
  airplay-scan --seconds 20 --json

Discovery does not prove playback compatibility. Devices on isolated Wi-Fi
or another VLAN may not be visible. Exit 1 means discovery failed.`;

type Service = {
  name: string; type: string; interface: string; protocol: string; domain: string;
  host?: string; address?: string; port?: number; txt?: string;
};

// Avahi escapes service names as decimal byte sequences, including UTF-8.
function unescapeName(value: string): string {
  const bytes: number[] = [];
  for (let i = 0; i < value.length;) {
    const match = /^\\(\d{3})/.exec(value.slice(i));
    if (match) { bytes.push(Number(match[1])); i += 4; }
    else {
      const char = String.fromCodePoint(value.codePointAt(i)!);
      bytes.push(...Buffer.from(char)); i += char.length;
    }
  }
  return Buffer.from(bytes).toString("utf8");
}

export function parseServices(output: string): Service[] {
  const services = new Map<string, Service>();
  for (const line of output.split("\n")) {
    const [event, iface, protocol, rawName, type, domain, host, address, port, ...txt] = line.trimEnd().split(";");
    if (!["+", "=", "-"].includes(event!) || !domain || !types.includes(type!)) continue;
    const key = JSON.stringify([iface, protocol, rawName, type, domain]);
    if (event === "-") { services.delete(key); continue; }
    const service: Service = { name: unescapeName(rawName!), type: type!, interface: iface!, protocol: protocol!, domain };
    if (event === "=") {
      const number = Number(port);
      if (!host || !address || !port || !Number.isInteger(number) || number < 1 || number > 65535) continue;
      Object.assign(service, { host, address, port: number, txt: txt.join(";") });
    } else if (services.has(key)) continue;
    services.set(key, service);
  }
  return [...services.values()].sort((a, b) => JSON.stringify(a).localeCompare(JSON.stringify(b)));
}

function secondsValue(value: string): number {
  const seconds = Number(value);
  if (!Number.isInteger(seconds) || seconds < 1 || seconds > 300) throw new Error("--seconds must be an integer from 1 to 300.");
  return seconds;
}

async function main() {
  const { values } = parseArgs({ options: {
    interface: { type: "string" }, seconds: { type: "string", default: "12" },
    json: { type: "boolean", default: false }, help: { type: "boolean", short: "h" },
  } });
  if (values.help) { console.log(help); return; }
  const seconds = secondsValue(values.seconds!);
  for (const executable of ["avahi-browse", "timeout"]) {
    if (!Bun.which(executable)) throw new Error(`${executable} is missing. Activate the NixOS AirPlay module, or run with avahi and coreutils on PATH.`);
  }
  const args = ["--parsable", "--resolve"];
  if (values.interface !== undefined) {
    if (!values.interface.trim()) throw new Error("--interface requires a network interface name.");
    args.push(`--interface=${values.interface}`);
  }
  if (!values.json) console.error(`Scanning for AirPlay services for ${seconds}s…`);
  const results = await Promise.all(types.map(async type => {
    const result = await $`timeout --signal=TERM --kill-after=2s ${`${seconds}s`} avahi-browse ${args} ${type}`.quiet().nothrow();
    const stderr = result.stderr.toString().trim();
    if (result.exitCode !== 0 && result.exitCode !== 124) {
      throw new Error(`${type}: ${stderr || `avahi-browse exited ${result.exitCode}`}. Check systemctl status avahi-daemon.`);
    }
    if (stderr) console.error(`${type}: ${stderr}`);
    return result.stdout.toString();
  }));
  const services = parseServices(results.join("\n"));
  if (values.json) console.log(JSON.stringify(services, null, 2));
  else if (!services.length) console.log("No AirPlay services discovered. Check receiver power, Wi-Fi isolation, and mDNS firewall rules (UDP 5353).");
  else for (const service of services) {
    console.log(`${JSON.stringify(service.name)} — ${service.type === "_raop._tcp" ? "RAOP audio" : "AirPlay"}`);
    console.log(`  ${service.interface} / ${service.protocol}: ${service.address ? `${service.protocol === "IPv6" ? `[${service.address}]` : service.address}:${service.port} (${service.host})` : "advertised, address unresolved"}`);
    if (service.txt) console.log(`  TXT: ${JSON.stringify(service.txt)}`);
  }
}

if (import.meta.main && process.env.NODE_ENV !== "test") {
  main().catch(error => { console.error(`airplay-scan: ${error.message}`); process.exitCode = 1; });
}

if (process.env.NODE_ENV === "test") {
  const { expect, test } = await import("bun:test");
  const added = "+;wlan0;IPv4;Living\\032Room;_airplay._tcp;local";
  const resolved = '=;wlan0;IPv4;Living\\032Room;_airplay._tcp;local;speaker.local;192.168.1.20;7000;"model=Speaker";"note=a;b"';
  test("merges advertisements with resolution, preserving TXT semicolons", () => {
    const result = parseServices([added, resolved, added, resolved].join("\n"));
    expect(result).toHaveLength(1);
    expect(result[0]).toEqual({ name: "Living Room", type: "_airplay._tcp", interface: "wlan0", protocol: "IPv4", domain: "local", host: "speaker.local", address: "192.168.1.20", port: 7000, txt: '"model=Speaker";"note=a;b"' });
  });
  test("retains unresolved services and removes withdrawn services", () => {
    expect(parseServices(added)[0]?.address).toBeUndefined();
    expect(parseServices(`${resolved}\n${added.replace(/^\+/, "-")}`)).toEqual([]);
  });
  test("ignores unrelated and malformed records without losing valid discoveries", () => {
    expect(parseServices(`${added}\n=;broken\n${resolved.replace("7000", "NaN")}\n${resolved.replace("_airplay", "_ssh")}`)).toEqual(parseServices(added));
  });
  test("keeps distinct interfaces and protocols", () => {
    expect(parseServices(`${resolved}\n${resolved.replaceAll("wlan0", "eth0")}\n${resolved.replace("IPv4", "IPv6").replace("192.168.1.20", "fe80::1")}`)).toHaveLength(3);
  });
  test("decodes escaped UTF-8 bytes and delimiter characters", () => {
    expect(unescapeName("Caf\\195\\169\\059\\092")).toBe("Café;\\");
  });
  test("rejects invalid scan durations", () => {
    for (const input of ["0", "-1", "NaN", "1.5", "301", ""]) expect(() => secondsValue(input)).toThrow();
    expect(secondsValue("12")).toBe(12);
  });
}
