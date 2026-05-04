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
 */

export type DetectResult = {
	theme: "dark" | "light";
	r: number;
	g: number;
	b: number;
	luminance: number;
};

const OSC11_RE = /\x1b\]11;rgb:([0-9a-fA-F]+)\/([0-9a-fA-F]+)\/([0-9a-fA-F]+)(\x1b\\|\x07)/;

/** Parse an OSC 11 response string into a theme result */
export function parseOsc11Response(raw: string): DetectResult | null {
	const match = raw.match(
		/\]11;rgb:([0-9a-fA-F]+)\/([0-9a-fA-F]+)\/([0-9a-fA-F]+)/,
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
 * Uses pi's onTerminalInput to intercept the OSC 11 response before
 * the TUI processes it. Much cleaner than monkey-patching stdin.
 */
export function queryTerminalBackground(
	onTerminalInput: TerminalInputInterceptor,
	timeoutMs = 2000,
): Promise<DetectResult | null> {
	return new Promise((resolve) => {
		let buf = "";
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

			buf += data;

			const match = buf.match(OSC11_RE);
			if (match) {
				cleanup();
				const result = parseOsc11Response(buf);
				resolve(result);

				// Return any non-OSC data that arrived alongside the response
				const cleaned = buf.replace(OSC11_RE, "");
				if (cleaned.length > 0) {
					return { consume: false, data: cleaned };
				}
				return { consume: true };
			}

			// Partial OSC response — consume and wait for more
			if (/\x1b\]/.test(buf)) {
				return { consume: true };
			}

			// Not our response — let it through and stop intercepting
			cleanup();
			return undefined;
		});

		// Send the query
		process.stdout.write("\x1b]11;?\x1b\\");
	});
}
