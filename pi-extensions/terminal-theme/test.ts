#!/usr/bin/env npx tsx
/**
 * Test suite for terminal background detection.
 *
 * Run: npx tsx ~/.pi/agent/extensions/terminal-theme/test.ts
 *
 * Tests:
 *   1. Parse known OSC 11 responses (offline, always runs)
 *   2. Interceptor behaviour with mocked input (offline, always runs)
 *   3. Live query against current terminal (needs TTY)
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

// ── Interceptor tests ──
//
// These use a mock interceptor that lets us drive arbitrary input
// sequences through queryTerminalBackground without a TTY. They exist
// to guarantee the interceptor never swallows non-OSC-11 data — that
// regression once caused user keystrokes (including Enter) to disappear.

type InterceptorHandler = (data: string) => { consume?: boolean; data?: string } | undefined;

interface MockInterceptor {
	interceptor: TerminalInputInterceptor;
	/** Feed data; returns what the TUI would have observed (passthrough). */
	feed: (data: string) => { passedThrough: string | null; consumed: boolean };
	/** True if a listener is currently registered. */
	hasListener: () => boolean;
}

function createMockInterceptor(): MockInterceptor {
	let handler: InterceptorHandler | null = null;
	return {
		interceptor: (h) => {
			handler = h;
			return () => {
				if (handler === h) handler = null;
			};
		},
		feed: (data) => {
			if (!handler) return { passedThrough: data, consumed: false };
			const result = handler(data);
			if (result?.consume) return { passedThrough: null, consumed: true };
			if (result?.data !== undefined) return { passedThrough: result.data, consumed: false };
			return { passedThrough: data, consumed: false };
		},
		hasListener: () => handler !== null,
	};
}

type InterceptorTest = { label: string; run: () => Promise<boolean> };

const interceptorTests: InterceptorTest[] = [
	{
		label: "resolves on OSC 11 (ST terminator) and consumes it",
		run: async () => {
			const mock = createMockInterceptor();
			const promise = queryTerminalBackground(mock.interceptor, 200);
			const { consumed } = mock.feed("\x1b]11;rgb:1c1c/2020/2828\x1b\\");
			const result = await promise;
			return consumed && result?.theme === "dark" && !mock.hasListener();
		},
	},
	{
		label: "resolves on OSC 11 (BEL terminator)",
		run: async () => {
			const mock = createMockInterceptor();
			const promise = queryTerminalBackground(mock.interceptor, 200);
			const { consumed } = mock.feed("\x1b]11;rgb:fd/f6/e3\x07");
			const result = await promise;
			return consumed && result?.theme === "light";
		},
	},
	{
		label: "passes regular keystrokes through (does not eat 'a')",
		run: async () => {
			const mock = createMockInterceptor();
			const promise = queryTerminalBackground(mock.interceptor, 50);
			const { passedThrough, consumed } = mock.feed("a");
			const result = await promise;
			return !consumed && passedThrough === "a" && result === null;
		},
	},
	{
		label: "passes Enter through (does not eat \\r)",
		run: async () => {
			const mock = createMockInterceptor();
			const promise = queryTerminalBackground(mock.interceptor, 50);
			const { passedThrough, consumed } = mock.feed("\r");
			const result = await promise;
			return !consumed && passedThrough === "\r" && result === null;
		},
	},
	{
		label: "passes unrelated OSC 7 (cwd) through",
		run: async () => {
			const mock = createMockInterceptor();
			const promise = queryTerminalBackground(mock.interceptor, 50);
			const osc7 = "\x1b]7;file:///tmp\x1b\\";
			const { passedThrough, consumed } = mock.feed(osc7);
			const result = await promise;
			return !consumed && passedThrough === osc7 && result === null;
		},
	},
	{
		label: "passes OSC 52 (clipboard) through",
		run: async () => {
			const mock = createMockInterceptor();
			const promise = queryTerminalBackground(mock.interceptor, 50);
			const osc52 = "\x1b]52;c;aGVsbG8=\x07";
			const { passedThrough, consumed } = mock.feed(osc52);
			const result = await promise;
			return !consumed && passedThrough === osc52 && result === null;
		},
	},
	{
		label: "passes OSC 10 (foreground) through — only OSC 11 is ours",
		run: async () => {
			const mock = createMockInterceptor();
			const promise = queryTerminalBackground(mock.interceptor, 50);
			const osc10 = "\x1b]10;rgb:ffff/ffff/ffff\x1b\\";
			const { passedThrough, consumed } = mock.feed(osc10);
			const result = await promise;
			return !consumed && passedThrough === osc10 && result === null;
		},
	},
	{
		label: "keystrokes before OSC 11 pass through; OSC 11 still resolves",
		run: async () => {
			const mock = createMockInterceptor();
			const promise = queryTerminalBackground(mock.interceptor, 200);
			const k1 = mock.feed("h");
			const k2 = mock.feed("i");
			const osc = mock.feed("\x1b]11;rgb:1c1c/2020/2828\x1b\\");
			const result = await promise;
			return (
				k1.passedThrough === "h" && !k1.consumed &&
				k2.passedThrough === "i" && !k2.consumed &&
				osc.consumed && result?.theme === "dark"
			);
		},
	},
	{
		label: "times out to null with no input; cleans up listener",
		run: async () => {
			const mock = createMockInterceptor();
			const result = await queryTerminalBackground(mock.interceptor, 30);
			return result === null && !mock.hasListener();
		},
	},
	{
		label: "after resolve, listener is removed (next input passes through)",
		run: async () => {
			const mock = createMockInterceptor();
			const promise = queryTerminalBackground(mock.interceptor, 200);
			mock.feed("\x1b]11;rgb:1c1c/2020/2828\x1b\\");
			await promise;
			const followUp = mock.feed("x");
			return (
				!mock.hasListener() &&
				followUp.passedThrough === "x" &&
				!followUp.consumed
			);
		},
	},
	{
		label: "after timeout, listener is removed (next input passes through)",
		run: async () => {
			const mock = createMockInterceptor();
			await queryTerminalBackground(mock.interceptor, 20);
			const followUp = mock.feed("x");
			return !mock.hasListener() && followUp.passedThrough === "x";
		},
	},
];

async function runInterceptorTests(): Promise<number> {
	console.log("── Interceptor tests ──\n");
	// Silence stdout while tests run, otherwise queryTerminalBackground's
	// raw OSC 11 query bytes pollute the test output.
	const origWrite = process.stdout.write.bind(process.stdout);
	process.stdout.write = (() => true) as typeof process.stdout.write;
	let passed = 0;
	try {
		for (const test of interceptorTests) {
			let ok = false;
			let error: unknown;
			try {
				ok = await test.run();
			} catch (err) {
				error = err;
			}
			const status = ok ? "✓" : "✗";
			origWrite(`  ${status} ${test.label}\n`);
			if (error) origWrite(`    ERROR: ${error}\n`);
			if (ok) passed++;
		}
	} finally {
		process.stdout.write = origWrite as typeof process.stdout.write;
	}
	console.log(`\n  ${passed}/${interceptorTests.length} passed\n`);
	return passed;
}

async function runOfflineOnly(interceptorPassed: number): Promise<void> {
	console.log("── Live probe ──\n");
	console.log(`  TERM:         ${process.env.TERM ?? "(unset)"}`);
	console.log(`  TERM_PROGRAM: ${process.env.TERM_PROGRAM ?? "(unset)"}`);
	console.log(`  SSH_TTY:      ${process.env.SSH_TTY ?? "(unset)"}`);
	console.log(`  stdin.isTTY:  ${process.stdin.isTTY ?? false}`);
	console.log(`  stdout.isTTY: ${process.stdout.isTTY ?? false}`);
	console.log("\n  ⚠ Not a TTY — skipping live probe");
	console.log("  Run directly in your terminal, not piped\n");
	const allOk =
		parsePassed === cases.length && interceptorPassed === interceptorTests.length;
	process.exit(allOk ? 0 : 1);
}

async function main(): Promise<void> {
	const interceptorPassed = await runInterceptorTests();
	if (!process.stdin.isTTY) {
		await runOfflineOnly(interceptorPassed);
		return;
	}
	await runLiveProbe(interceptorPassed);
}

// ── Live terminal probe ──

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

async function runLiveProbe(interceptorPassed: number): Promise<void> {
	console.log("── Live probe ──\n");
	console.log(`  TERM:         ${process.env.TERM ?? "(unset)"}`);
	console.log(`  TERM_PROGRAM: ${process.env.TERM_PROGRAM ?? "(unset)"}`);
	console.log(`  SSH_TTY:      ${process.env.SSH_TTY ?? "(unset)"}`);
	console.log(`  stdin.isTTY:  ${process.stdin.isTTY ?? false}`);
	console.log(`  stdout.isTTY: ${process.stdout.isTTY ?? false}`);

	// Need raw mode for the query to work
	const wasRaw = process.stdin.isRaw;
	process.stdin.setRawMode(true);
	process.stdin.resume();

	console.log("\n  Querying terminal background...");

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
	const offlinePassed =
		parsePassed === cases.length && interceptorPassed === interceptorTests.length;
	if (offlinePassed && detected) {
		console.log("All tests passed ✓");
	} else if (offlinePassed) {
		console.log("Offline tests passed, live probe failed (may need TTY)");
	} else {
		console.log("FAILURES detected");
	}
	process.exit(offlinePassed ? 0 : 1);
}

main();
