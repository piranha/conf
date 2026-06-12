/**
 * Syncs pi theme with terminal background color.
 * Works over SSH — probes terminal via OSC 11 escape sequence.
 * Tested with Ghostty, iTerm2, kitty, foot, and most modern terminals.
 *
 * Uses pi's terminal_input interception to cleanly consume the OSC 11
 * response before the TUI sees it as garbage input.
 *
 * Re-probe strategy:
 *   - Once at session start.
 *   - After each agent turn ends (agent_end), with a cooldown.
 *   - Manually via /theme-detect.
 *
 * agent_end is a safe moment: the agent just finished, user is not
 * (yet) typing into the editor, so probing can't race with keystrokes.
 *
 * We deliberately do NOT hook a persistent keystroke listener — that
 * approach interferes with pi-tui's input pipeline because newly added
 * listeners are visited within the same Set iteration as the keystroke
 * that triggered them.
 *
 * Test: npx tsx ~/.pi/agent/extensions/terminal-theme/test.ts
 */

import type { ExtensionAPI, ExtensionUIContext } from "@mariozechner/pi-coding-agent";
import { queryTerminalBackground } from "./detect.js";

const COOLDOWN_MS = 30_000;

async function detect(ui: ExtensionUIContext): Promise<"dark" | "light" | null> {
	const result = await queryTerminalBackground(
		(handler) => ui.onTerminalInput(handler),
	);
	if (result) {
		ui.setTheme(result.theme);
		return result.theme;
	}
	return null;
}

export default function (pi: ExtensionAPI) {
	let lastProbe = 0;
	let probing = false;
	let shuttingDown = false;
	let currentProbe: Promise<"dark" | "light" | null> | null = null;

	const runDetect = async (ui: ExtensionUIContext): Promise<"dark" | "light" | null> => {
		if (shuttingDown) return null;
		const probe = detect(ui);
		currentProbe = probe;
		try {
			return await probe;
		} finally {
			if (currentProbe === probe) currentProbe = null;
		}
	};

	const maybeProbe = async (ui: ExtensionUIContext) => {
		const now = Date.now();
		if (shuttingDown || probing || now - lastProbe < COOLDOWN_MS) return;
		probing = true;
		lastProbe = now;
		try {
			await runDetect(ui);
		} finally {
			probing = false;
		}
	};

	pi.on("session_start", async (_event, ctx) => {
		if (shuttingDown) return;
		probing = true;
		try {
			await runDetect(ctx.ui);
		} finally {
			probing = false;
			lastProbe = Date.now();
		}
	});

	// Re-probe after the agent finishes a turn — safe moment, no
	// concurrent user input flying through stdin.
	pi.on("agent_end", async (_event, ctx) => {
		await maybeProbe(ctx.ui);
	});

	pi.on("session_shutdown", async () => {
		shuttingDown = true;

		// If an OSC 11 probe is already in flight, give its interceptor a
		// short chance to consume the terminal response before TUI teardown.
		if (currentProbe) {
			await Promise.race([
				currentProbe.catch(() => null),
				new Promise((resolve) => setTimeout(resolve, 600)),
			]);
		}
	});

	pi.registerCommand("theme-detect", {
		description: "Re-detect terminal background and switch theme",
		handler: async (_args, ctx) => {
			if (shuttingDown) return;
			probing = true;
			let theme: "dark" | "light" | null = null;
			try {
				theme = await runDetect(ctx.ui);
			} finally {
				probing = false;
				lastProbe = Date.now();
			}
			if (theme) {
				ctx.ui.notify(`Detected ${theme} background`, "info");
			} else {
				ctx.ui.notify("Could not detect terminal background (OSC 11 not supported?)", "warning");
			}
		},
	});
}
