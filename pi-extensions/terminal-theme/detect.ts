/**
 * Terminal background color detection via OSC 11.
 *
 * OSC 11 query: \e]11;?\e\\
 * Response:     \e]11;rgb:RRRR/GGGG/BBBB\e\\  (or \a terminator)
 *
 * Color components can be 1-4 hex digits depending on terminal.
 * We normalize to 8-bit by taking the first 2 digits.
 *
 * Uses pi's terminal_input interception to cleanly consume the OSC 11
 * response before it reaches the TUI as garbage input.
 *
 * The interceptor is intentionally STRICT: it only consumes data that
 * looks exactly like an OSC 11 response. Anything else is passed
 * through untouched so we never swallow user keystrokes or unrelated
 * terminal escape sequences (OSC 7, OSC 52, focus events, etc.).
 *
 * pi-tui's StdinBuffer guarantees that complete escape sequences are
 * delivered as single events, so we don't need to accumulate partial
 * data across multiple calls.
 */

export type DetectResult = {
	theme: "dark" | "light";
	r: number;
	g: number;
	b: number;
	luminance: number;
};

/** Parse an OSC 11 response string into a theme result */
export function parseOsc11Response(raw: string): DetectResult | null {
	const match = raw.match(
		/\x1b\]11;rgb:([0-9a-fA-F]+)\/([0-9a-fA-F]+)\/([0-9a-fA-F]+)/,
	);
	if (!match) return null;

	const norm = (hex: string): number => {
		if (hex.length === 1) return parseInt(hex + hex, 16);
		return parseInt(hex.substring(0, 2), 16);
	};

	const r = norm(match[1]);
	const g = norm(match[2]);
	const b = norm(match[3]);
	const luminance = 0.2126 * r + 0.7152 * g + 0.0722 * b;

	return { theme: luminance < 128 ? "dark" : "light", r, g, b, luminance };
}

export type TerminalInputInterceptor = (
	handler: (data: string) => { consume?: boolean; data?: string } | undefined,
) => () => void;

/**
 * Query the terminal for its background color.
 *
 * Sends OSC 11 and waits for a matching response. The interceptor only
 * consumes data that parses as an OSC 11 response; everything else is
 * passed through unchanged. The listener stays installed until either
 * a response arrives or the timeout elapses.
 */
export function queryTerminalBackground(
	onTerminalInput: TerminalInputInterceptor,
	timeoutMs = 500,
): Promise<DetectResult | null> {
	return new Promise((resolve) => {
		let done = false;

		const cleanup = () => {
			if (done) return;
			done = true;
			unsubscribe();
			clearTimeout(timer);
		};

		const timer = setTimeout(() => {
			cleanup();
			resolve(null);
		}, timeoutMs);

		const unsubscribe = onTerminalInput((data: string) => {
			if (done) return undefined;

			const result = parseOsc11Response(data);
			if (result) {
				cleanup();
				resolve(result);
				return { consume: true };
			}

			// Not an OSC 11 response — pass through, keep listening.
			return undefined;
		});

		// Send the query
		process.stdout.write("\x1b]11;?\x1b\\");
	});
}
