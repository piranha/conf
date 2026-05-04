/**
 * Thinking Per Model Extension
 *
 * Remembers and restores the thinking level for each model.
 * When you switch models, the thinking level you last used with
 * that model is automatically restored.
 *
 * State is saved to ~/.pi/agent/thinking-levels.json (global, cross-session).
 *
 * Usage: Place in ~/.pi/agent/extensions/
 */

import { existsSync, readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { join, dirname } from "node:path";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { getAgentDir } from "@mariozechner/pi-coding-agent";

type ThinkingLevel = "off" | "minimal" | "low" | "medium" | "high" | "xhigh";

interface ThinkingLevels {
	[modelKey: string]: ThinkingLevel;
}

const CONFIG_FILE = join(getAgentDir(), "thinking-levels.json");

function loadLevels(): ThinkingLevels {
	if (!existsSync(CONFIG_FILE)) return {};
	try {
		return JSON.parse(readFileSync(CONFIG_FILE, "utf-8"));
	} catch {
		return {};
	}
}

function saveLevels(levels: ThinkingLevels): void {
	try {
		mkdirSync(dirname(CONFIG_FILE), { recursive: true });
		writeFileSync(CONFIG_FILE, JSON.stringify(levels, null, 2), "utf-8");
	} catch (err) {
		console.error(`[thinking-per-model] Failed to save: ${err}`);
	}
}

function modelKey(provider: string, id: string): string {
	return `${provider}/${id}`;
}

export default function (pi: ExtensionAPI) {
	let levels = loadLevels();
	let currentModelKey: string | undefined;
	// Cache the last known thinking level so we can save it even after
	// pi has already clamped the level for a newly selected model.
	let cachedLevel: ThinkingLevel | undefined;
	let suppressSave = false;

	// Save cached thinking level for the current model
	function saveCurrentLevel() {
		if (!currentModelKey || suppressSave || cachedLevel === undefined) return;
		if (levels[currentModelKey] !== cachedLevel) {
			levels[currentModelKey] = cachedLevel;
			saveLevels(levels);
		}
	}

	// Snapshot the current thinking level into cache
	function snapshotLevel() {
		if (!suppressSave) {
			cachedLevel = pi.getThinkingLevel();
		}
	}

	pi.on("model_select", async (event, ctx) => {
		// Save outgoing model's thinking level using the cached value
		// (pi may have already clamped the level for the new model)
		saveCurrentLevel();

		const key = modelKey(event.model.provider, event.model.id);
		currentModelKey = key;

		const saved = levels[key];
		if (saved !== undefined && event.source !== "restore") {
			// Restore saved thinking level for this model
			suppressSave = true;
			pi.setThinkingLevel(saved);
			suppressSave = false;
			cachedLevel = pi.getThinkingLevel();
			ctx.ui.notify(`Thinking: ${cachedLevel} (restored for ${event.model.id})`, "info");
		} else {
			// No saved level — snapshot whatever pi set
			cachedLevel = pi.getThinkingLevel();
		}
	});

	// Capture thinking level before each turn so we persist manual changes
	pi.on("turn_start", async () => {
		snapshotLevel();
		saveCurrentLevel();
	});

	// Also capture on agent end (covers cases with no tool calls)
	pi.on("agent_end", async () => {
		snapshotLevel();
		saveCurrentLevel();
	});

	// Save on shutdown
	pi.on("session_shutdown", async () => {
		snapshotLevel();
		saveCurrentLevel();
	});
}
