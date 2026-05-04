/**
 * Agent Timer Extension
 *
 * Shows elapsed time in the footer status line while the agent is working.
 */

import type { ExtensionAPI, ExtensionContext } from "@mariozechner/pi-coding-agent";

function formatDuration(ms: number): string {
	const totalSec = Math.floor(ms / 1000);
	const min = Math.floor(totalSec / 60);
	const sec = totalSec % 60;
	if (min > 0) {
		return `${min}m${sec.toString().padStart(2, "0")}s`;
	}
	return `${sec}s`;
}

export default function (pi: ExtensionAPI) {
	let startTime: number | null = null;
	let timer: ReturnType<typeof setInterval> | null = null;

	function stopTimer() {
		if (timer) {
			clearInterval(timer);
			timer = null;
		}
	}

	function updateStatus(ctx: ExtensionContext, running: boolean) {
		if (startTime === null) return;
		const elapsed = Date.now() - startTime;
		const theme = ctx.ui.theme;
		const icon = running ? theme.fg("accent", "⏱") : theme.fg("success", "⏱");
		const time = running ? theme.fg("dim", ` ${formatDuration(elapsed)}`) : theme.fg("success", ` ${formatDuration(elapsed)}`);
		ctx.ui.setStatus("agent-timer", icon + time);
	}

	pi.on("agent_start", async (_event, ctx) => {
		startTime = Date.now();
		stopTimer();
		updateStatus(ctx, true);
		timer = setInterval(() => updateStatus(ctx, true), 1000);
	});

	pi.on("agent_end", async (_event, ctx) => {
		stopTimer();
		updateStatus(ctx, false);
	});

	pi.on("session_switch", async (_event, ctx) => {
		stopTimer();
		startTime = null;
		ctx.ui.setStatus("agent-timer", undefined);
	});

	pi.on("session_shutdown", async (_event, _ctx) => {
		stopTimer();
	});
}
