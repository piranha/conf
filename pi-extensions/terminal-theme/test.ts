#!/usr/bin/env npx tsx
/**
 * Test suite for terminal background detection.
 *
 * Run: npx tsx ~/.pi/agent/extensions/terminal-theme/test.ts
 *
 * Tests:
 *   1. Parse known OSC 11 responses (offline, always runs)
 *   2. Live query against current terminal (needs TTY)
 */

import { parseOsc11Response, queryTerminalBackground, type TerminalInputInterceptor } from "./detect.js";

// ── Offline parse tests ──

const cases: { input: string; theme: string; label: string }[] = [
	{
		label: "4-digit hex dark (ghostty)",
		input: "\x1b]11;rgb:1c1c/2020/2828\x1b\\",
		theme: "dark",
	},
	{
		label: "4-digit hex light",
		input: "\x1b]11;rgb:ffff/ffff/ffff\x1b\\",
		theme: "light",
	},
	{
		label: "2-digit hex dark",
		input: "\x1b]11;rgb:1c/20/28\x1b\\",
		theme: "dark",
	},
	{
		label: "2-digit hex light (solarized)",
		input: "\x1b]11;rgb:fd/f6/e3\x07",
		theme: "light",
	},
	{
		label: "pure black",
		input: "\x1b]11;rgb:0000/0000/0000\x1b\\",
		theme: "dark",
	},
	{
		label: "pure white (BEL terminator)",
		input: "\x1b]11;rgb:ffff/ffff/ffff\x07",
		theme: "light",
	},
	{
		label: "dracula",
		input: "\x1b]11;rgb:2828/2a2a/3636\x1b\\",
		theme: "dark",
	},
	{
		label: "gruvbox light",
		input: "\x1b]11;rgb:fbfb/f1f1/c7c7\x1b\\",
		theme: "light",
	},
	{
		label: "1-digit hex",
		input: "\x1b]11;rgb:0/0/0\x1b\\",
		theme: "dark",
	},
	{
		label: "garbage",
		input: "not an osc response",
		theme: "null",
	},
	{
		label: "partial response",
		input: "\x1b]11;rgb:ff",
		theme: "null",
	},
];

console.log("── Parse tests ──\n");
let parsePassed = 0;
for (const { input, theme, label } of cases) {
	const result = parseOsc11Response(input);
	const actual = result ? result.theme : "null";
	const ok = actual === theme;
	const status = ok ? "✓" : "✗";
	const detail = result
		? `rgb(${result.r},${result.g},${result.b}) lum=${result.luminance.toFixed(1)}`
		: "no match";
	console.log(
		`  ${status} ${label.padEnd(35)} => ${actual.padEnd(6)} (${detail})`,
	);
	if (!ok) console.log(`    EXPECTED: ${theme}`);
	else parsePassed++;
}
console.log(`\n  ${parsePassed}/${cases.length} passed\n`);

// ── Live terminal probe ──

console.log("── Live probe ──\n");
console.log(`  TERM:         ${process.env.TERM ?? "(unset)"}`);
console.log(`  TERM_PROGRAM: ${process.env.TERM_PROGRAM ?? "(unset)"}`);
console.log(`  SSH_TTY:      ${process.env.SSH_TTY ?? "(unset)"}`);
console.log(`  stdin.isTTY:  ${process.stdin.isTTY ?? false}`);
console.log(`  stdout.isTTY: ${process.stdout.isTTY ?? false}`);

if (!process.stdin.isTTY) {
	console.log("\n  ⚠ Not a TTY — skipping live probe");
	console.log("  Run directly in your terminal, not piped\n");
	process.exit(parsePassed === cases.length ? 0 : 1);
}

// For standalone testing, create a stdin-based interceptor that mimics
// pi's onTerminalInput API.
const createStdinInterceptor = (): TerminalInputInterceptor => {
	return (handler) => {
		const onData = (chunk: Buffer) => {
			const data = chunk.toString();
			const result = handler(data);
			if (result?.consume) {
				// Consumed — don't pass through
			} else if (result?.data !== undefined) {
				// Transformed data — would pass through in pi
				process.stdout.write(`[passthrough: ${JSON.stringify(result.data)}]`);
			}
			// undefined = not intercepted
		};
		process.stdin.on("data", onData);
		return () => {
			process.stdin.removeListener("data", onData);
		};
	};
};

// Need raw mode for the query to work
const wasRaw = process.stdin.isRaw;
process.stdin.setRawMode(true);
process.stdin.resume();

console.log("\n  Querying terminal background...");

async function runLiveProbe() {
	const detected = await queryTerminalBackground(createStdinInterceptor(), 3000);

	process.stdin.setRawMode(wasRaw ?? false);

	if (detected) {
		console.log(`  ✓ Detected!`);
		console.log(`    Background: rgb(${detected.r}, ${detected.g}, ${detected.b})`);
		console.log(`    Luminance:  ${detected.luminance.toFixed(1)}`);
		console.log(`    Theme:      ${detected.theme}`);
	} else {
		console.log("  ✗ No response — terminal may not support OSC 11");
		console.log("    Supported: Ghostty, iTerm2, kitty, foot, WezTerm, Alacritty");
		console.log("    Not supported: Apple Terminal, some old xterm configs");
	}

	console.log();
	const allPassed = parsePassed === cases.length;
	if (allPassed && detected) {
		console.log("All tests passed ✓");
	} else if (allPassed) {
		console.log("Parse tests passed, live probe failed (may need TTY)");
	} else {
		console.log("FAILURES detected");
	}
	process.exit(allPassed ? 0 : 1);
}

runLiveProbe();
