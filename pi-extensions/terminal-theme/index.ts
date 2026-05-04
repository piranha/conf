/**
 * Syncs pi theme with terminal background color.
 * Works over SSH — probes terminal via OSC 11 escape sequence.
 * Tested with Ghostty, iTerm2, kitty, foot, and most modern terminals.
 *
 * Uses pi's terminal_input interception to cleanly consume the OSC 11
 * response before the TUI sees it as garbage input.
 *
 * Detects at startup, then re-probes on any keystroke after a cooldown
 * period (default 30s). This catches terminal theme changes without
 * polling — just switch your theme and start typing.
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

	pi.on("session_start", async (_event, ctx) => {
		// Detect once at startup
		await detect(ctx.ui);
		lastProbe = Date.now();

		// Re-probe on keystroke activity after cooldown
		ctx.ui.onTerminalInput(() => {
			const now = Date.now();
			if (!probing && now - lastProbe > COOLDOWN_MS) {
				probing = true;
				lastProbe = now;
				detect(ctx.ui).finally(() => { probing = false; });
			}
			return undefined; // always pass input through
		});
	});

	pi.registerCommand("theme-detect", {
		description: "Re-detect terminal background and switch theme",
		handler: async (_args, ctx) => {
			const theme = await detect(ctx.ui);
			lastProbe = Date.now();
			if (theme) {
				ctx.ui.notify(`Detected ${theme} background`, "info");
			} else {
				ctx.ui.notify("Could not detect terminal background (OSC 11 not supported?)", "warning");
			}
		},
	});
}
