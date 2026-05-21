/**
 * /push — synthesize a "push to GitHub main" turn.
 *
 * Records into the session as if the user had asked, and the model had
 * decided to run a bash tool call:
 *
 *   user:        "Please push this worktree to GitHub main"
 *   assistant:   toolCall bash { command: "git push origin main" }
 *   toolResult:  <stdout/stderr/exit code>
 *
 * The extension itself executes the push (via pi.exec), then triggers a
 * follow-up turn so the model sees the tool result and reports back to
 * the user in natural language.
 */

import type { ExtensionAPI, SessionManager } from "@mariozechner/pi-coding-agent";

const COMMAND = "git push origin main";

export default function (pi: ExtensionAPI) {
	pi.registerCommand("push", {
		description: "Push current worktree to GitHub main (synthesizes a bash tool call)",
		handler: async (_args, ctx) => {
			// Need an interactive-ish session; refuse if agent is busy.
			if (!ctx.isIdle()) {
				ctx.ui.notify("Agent is busy — try /push again when idle.", "warning");
				return;
			}
			await ctx.waitForIdle();

			// ExtensionContext.sessionManager is typed as ReadonlySessionManager, but
			// the underlying object is the full SessionManager. We need write access
			// to inject the synthetic assistant tool call + tool result. (See
			// pi-coding-agent dist/core/extensions/runner.js: createContext.)
			const sm = ctx.sessionManager as unknown as SessionManager;
			const model = ctx.model;

			// 1. Synthesize the user message.
			sm.appendMessage({
				role: "user",
				content: "Please push this worktree to GitHub main",
				timestamp: Date.now(),
			});

			// 2. Run the push. We do this ourselves rather than asking the LLM,
			//    which is the whole point of /push as a shortcut.
			ctx.ui.setStatus("push", "git push origin main…");
			const result = await pi.exec("bash", ["-lc", COMMAND], { cwd: ctx.cwd });
			ctx.ui.setStatus("push", undefined);

			const stdout = (result.stdout ?? "").trimEnd();
			const stderr = (result.stderr ?? "").trimEnd();
			const combined =
				[stdout, stderr].filter((s) => s.length > 0).join("\n") ||
				"(no output)";
			const isError = result.code !== 0;

			// 3. Synthesize the assistant message containing the bash tool call.
			//    stopReason: "toolUse" so the conversation shape matches a real
			//    tool-using turn.
			const toolCallId = `push_${Date.now().toString(36)}`;
			const zeroUsage = {
				input: 0,
				output: 0,
				cacheRead: 0,
				cacheWrite: 0,
				totalTokens: 0,
				cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, total: 0 },
			};
			sm.appendMessage({
				role: "assistant",
				content: [
					{
						type: "toolCall",
						id: toolCallId,
						name: "bash",
						arguments: { command: COMMAND },
					},
				],
				api: (model?.api as any) ?? "synthetic",
				provider: (model?.provider as any) ?? "synthetic",
				model: model?.id ?? "synthetic",
				usage: zeroUsage,
				stopReason: "toolUse",
				timestamp: Date.now(),
			} as any);

			// 4. Synthesize the tool result so the model sees what happened.
			//    `details` mirrors the shape of the real bash tool's details
			//    so existing renderers/extensions don't choke.
			sm.appendMessage({
				role: "toolResult",
				toolCallId,
				toolName: "bash",
				content: [{ type: "text", text: combined }],
				details: {
					command: COMMAND,
					exitCode: result.code,
					output: combined,
					cancelled: false,
					truncated: false,
				},
				isError,
				timestamp: Date.now(),
			});

			ctx.ui.notify(
				isError ? `Push failed (exit ${result.code})` : "Pushed to origin/main",
				isError ? "error" : "info",
			);

			// 5. Hand control to the model so it can speak about the result.
			//    A hidden custom message with triggerTurn:true is the only
			//    extension-API way to kick the agent loop without typing
			//    something visible.
			pi.sendMessage(
				{
					customType: "vyriy/push-trigger",
					content: "",
					display: false,
				},
				{ triggerTurn: true },
			);
		},
	});
}
