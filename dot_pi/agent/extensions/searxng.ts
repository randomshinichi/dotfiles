/**
 * SearXNG Web Search Extension for pi
 *
 * Registers a `search` tool so the LLM can query your local SearXNG instance.
 *
 * Installation (pick one):
 *   Global  – copy to ~/.pi/agent/extensions/searxng.ts
 *   Project – copy to .pi/extensions/searxng.ts
 *
 * Test without installing:
 *   pi -e ./searxng.ts
 *
 * Requirements:
 *   Your SearXNG instance must have `json` in its search.formats list (settings.yml).
 *   Verify with:  curl 'http://localhost:8888/search?q=test&format=json'
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import {
	DEFAULT_MAX_BYTES,
	DEFAULT_MAX_LINES,
	formatSize,
	truncateHead,
} from "@mariozechner/pi-coding-agent";
import { StringEnum } from "@mariozechner/pi-ai";
import { Text } from "@mariozechner/pi-tui";
import { Type } from "@sinclair/typebox";

// ---------------------------------------------------------------------------
// Config
// ---------------------------------------------------------------------------

const SEARXNG_BASE_URL = "http://localhost:8888";
const MAX_RESULTS = 10; // How many results to forward to the LLM

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

interface SearxngResult {
	title?: string;
	url?: string;
	content?: string; // snippet
	engine?: string;
}

interface SearxngResponse {
	query: string;
	results: SearxngResult[];
	answers?: string[];
	infoboxes?: Array<{ infobox: string; content: string }>;
	suggestions?: string[];
	number_of_results?: number;
}

interface SearchDetails {
	query: string;
	total: number;
	returned: number;
	truncated: boolean;
	error?: string;
}

// ---------------------------------------------------------------------------
// Schema
// Use StringEnum (not Type.Union/Literal) for Google model compatibility.
// ---------------------------------------------------------------------------

const SearchParams = Type.Object({
	query: Type.String({
		description:
			"The search query. Engine-specific syntax is supported, e.g. `site:github.com pi-agent`.",
	}),
	categories: Type.Optional(
		Type.String({
			description:
				"Comma-separated search categories. Common values: general, images, news, science, it, files.",
		})
	),
	engines: Type.Optional(
		Type.String({
			description: "Comma-separated engines to use, e.g. `google,bing,wikipedia`.",
		})
	),
	language: Type.Optional(
		Type.String({
			description: "BCP 47 language code, e.g. `en`, `de`, `fr`.",
		})
	),
	time_range: Type.Optional(
		StringEnum(["day", "month", "year"] as const, {
			description: "Restrict results to the last day, month, or year (engine-dependent).",
		})
	),
	pageno: Type.Optional(
		Type.Number({
			description: "Result page number. Defaults to 1.",
			default: 1,
			minimum: 1,
		})
	),
	safesearch: Type.Optional(
		StringEnum(["0", "1", "2"] as const, {
			description: "Safe-search level: 0 = off, 1 = moderate, 2 = strict.",
		})
	),
});

// ---------------------------------------------------------------------------
// Extension entry point
// ---------------------------------------------------------------------------

export default function (pi: ExtensionAPI) {
	pi.registerTool({
		name: "search",
		label: "Web Search",
		description: `Search the web via a local SearXNG instance (${SEARXNG_BASE_URL}).
Use this to look up current information, documentation, error messages, or anything not in your training data.
Returns titles, URLs, and text snippets.
Output is truncated to ${DEFAULT_MAX_LINES} lines or ${formatSize(DEFAULT_MAX_BYTES)}.`,

		parameters: SearchParams,

		async execute(_toolCallId, params, _signal, _onUpdate, _ctx) {
			// Build request URL
			const url = new URL("/search", SEARXNG_BASE_URL);
			url.searchParams.set("q", params.query);
			url.searchParams.set("format", "json");

			if (params.categories) url.searchParams.set("categories", params.categories);
			if (params.engines) url.searchParams.set("engines", params.engines);
			if (params.language) url.searchParams.set("language", params.language);
			if (params.time_range) url.searchParams.set("time_range", params.time_range);
			if (params.pageno) url.searchParams.set("pageno", String(params.pageno));
			if (params.safesearch !== undefined)
				url.searchParams.set("safesearch", params.safesearch);

			// Fetch
			let data: SearxngResponse;
			try {
				const response = await fetch(url.toString(), {
					headers: { Accept: "application/json" },
				});

				if (!response.ok) {
					const body = await response.text().catch(() => "");
					const msg = `SearXNG returned HTTP ${response.status}${body ? `: ${body}` : ""}`;
					return {
						content: [{ type: "text", text: msg }],
						details: {
							query: params.query,
							total: 0,
							returned: 0,
							truncated: false,
							error: msg,
						} satisfies SearchDetails,
					};
				}

				data = (await response.json()) as SearxngResponse;
			} catch (err: any) {
				const msg = `Could not reach SearXNG at ${SEARXNG_BASE_URL}: ${err.message}`;
				return {
					content: [{ type: "text", text: msg }],
					details: {
						query: params.query,
						total: 0,
						returned: 0,
						truncated: false,
						error: msg,
					} satisfies SearchDetails,
				};
			}

			// Build text output
			const parts: string[] = [];

			if (data.answers?.length) {
				parts.push(`**Direct answer:** ${data.answers.join(" | ")}`);
			}

			for (const box of data.infoboxes?.slice(0, 2) ?? []) {
				parts.push(`**${box.infobox}**\n${box.content}`);
			}

			const results = data.results?.slice(0, MAX_RESULTS) ?? [];
			if (results.length === 0 && parts.length === 0) {
				return {
					content: [{ type: "text", text: "No results found." }],
					details: {
						query: params.query,
						total: 0,
						returned: 0,
						truncated: false,
					} satisfies SearchDetails,
				};
			}

			results.forEach((r, i) => {
				const lines = [`${i + 1}. **${r.title ?? "(no title)"}**`, `   ${r.url ?? ""}`];
				if (r.content) lines.push(`   ${r.content}`);
				parts.push(lines.join("\n"));
			});

			if (data.suggestions?.length) {
				parts.push(`**Related:** ${data.suggestions.slice(0, 5).join(", ")}`);
			}

			// Truncate so we never blow the context window
			const raw = parts.join("\n\n");
			const truncation = truncateHead(raw, {
				maxLines: DEFAULT_MAX_LINES,
				maxBytes: DEFAULT_MAX_BYTES,
			});

			let text = truncation.content;
			if (truncation.truncated) {
				text += `\n\n[Output truncated: ${truncation.outputLines}/${truncation.totalLines} lines shown.]`;
			}

			return {
				content: [{ type: "text", text }],
				details: {
					query: params.query,
					total: data.number_of_results ?? results.length,
					returned: results.length,
					truncated: truncation.truncated,
				} satisfies SearchDetails,
			};
		},

		// --- Custom TUI rendering ---

		renderCall(args, theme) {
			let text = theme.fg("toolTitle", theme.bold("search "));
			text += theme.fg("accent", `"${args.query}"`);
			if (args.categories) text += theme.fg("dim", ` [${args.categories}]`);
			if (args.time_range) text += theme.fg("dim", ` (${args.time_range})`);
			return new Text(text, 0, 0);
		},

		renderResult(result, { expanded, isPartial }, theme) {
			const d = result.details as SearchDetails | undefined;

			if (isPartial) return new Text(theme.fg("warning", "Searching…"), 0, 0);

			if (d?.error) return new Text(theme.fg("error", `✗ ${d.error}`), 0, 0);

			if (!d || d.returned === 0)
				return new Text(theme.fg("dim", "No results found"), 0, 0);

			let text = theme.fg("success", `${d.returned} result${d.returned !== 1 ? "s" : ""}`);
			if (d.total > d.returned)
				text += theme.fg("dim", ` of ~${d.total.toLocaleString()}`);
			if (d.truncated) text += theme.fg("warning", " (truncated)");

			if (expanded) {
				const content = result.content[0];
				if (content?.type === "text") {
					for (const line of content.text.split("\n").slice(0, 30)) {
						text += `\n${theme.fg("dim", line)}`;
					}
				}
			}

			return new Text(text, 0, 0);
		},
	});
}
