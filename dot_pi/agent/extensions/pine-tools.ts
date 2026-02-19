import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import {
	DEFAULT_MAX_BYTES,
	DEFAULT_MAX_LINES,
	truncateHead,
} from "@mariozechner/pi-coding-agent";
import { StringEnum } from "@mariozechner/pi-ai";
import { Text, Markdown } from "@mariozechner/pi-tui";
import { Type } from "@sinclair/typebox";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// The repo location - configured via environment variable
// Set PINE_TOOLS_DIR to the location of the pine-tools repository
const REPO_DIR = process.env.PINE_TOOLS_DIR || "/data/source/projects/ai-tools/mcp/pine-tools";
const LANGUAGE_SERVICE_PATH = path.join(REPO_DIR, "dist/packages/language-service/index.js");
const PINE_DATA_PATH = path.join(REPO_DIR, "dist/pine-data/v6/index.js");

export default function (pi: ExtensionAPI) {
	// Dynamically import the language service to support different repo locations
	let languageService: any;
	let PineV6: any;

	async function getServices() {
		if (!languageService) {
			const { PineLanguageService: PLS } = await import(LANGUAGE_SERVICE_PATH);
			const { PineV6: PV6 } = await import(PINE_DATA_PATH);
			languageService = PLS;
			PineV6 = PV6;
		}
		return { PineLanguageService: languageService, PineV6 };
	}

	// ========== TOOLS (MCP-like capabilities) ==========

	/**
	 * Validate Pine Script code and return errors/warnings
	 */

	/**
	 * Look up documentation for a Pine Script symbol
	 */
	pi.registerTool({
		name: "pine_lookup",
		label: "Look up Pine Symbol",
		description:
			"Look up documentation for a Pine Script symbol (function, variable, or constant). Examples: 'ta.sma', 'close', 'plot', 'color.red'",
		parameters: Type.Object({
			symbol: Type.String({
				description:
					"Symbol name to look up (e.g., 'ta.sma', 'close', 'plot', 'color.red')",
			}),
		}),
		async execute(toolCallId, params, signal) {
			if (signal?.aborted) {
				return { content: [{ type: "text", text: "Cancelled." }], details: {} };
			}

			try {
				const { PineLanguageService } = await getServices();
				const info = PineLanguageService.getSymbolInfo(params.symbol);

				if (!info) {
					return {
						content: [
							{
								type: "text",
								text: JSON.stringify(
									{
										found: false,
										message: `Symbol '${params.symbol}' not found`,
									},
									null,
									2,
								),
							},
						],
						details: { found: false },
					};
				}

				return {
					content: [
						{
							type: "text",
							text: JSON.stringify(
								{
									found: true,
									name: info.name,
									kind: info.kind,
									syntax: info.syntax,
									description: info.description,
									returns: info.returns,
									type: info.type,
									parameters: info.parameters,
									namespace: info.namespace,
									deprecated: info.deprecated,
								},
								null,
								2,
							),
						},
					],
					details: { found: true, name: info.name },
				};
			} catch (err: any) {
				return {
					content: [
						{
							type: "text",
							text: `Error: ${err.message}`,
						},
					],
					details: { error: err.message },
				};
			}
		},
		renderCall: (args, theme) =>
			new Text(
				theme.fg("toolTitle", theme.bold("pine_lookup ")) +
					theme.fg("accent", args.symbol),
				0,
				0,
			),
		renderResult: (result, { isPartial }, theme) => {
			if (isPartial) return new Text(theme.fg("warning", "Looking up…"), 0, 0);
			if (!result.details?.found) {
				return new Text(theme.fg("dim", "Symbol not found"), 0, 0);
			}
			return new Text(
				theme.fg("success", "✓ ") + theme.fg("accent", result.details.name),
				0,
			);
		},
	});

	/**
	 * List Pine Script functions, optionally filtered by namespace
	 */
	pi.registerTool({
		name: "pine_list_functions",
		label: "List Pine Functions",
		description:
			"List available Pine Script functions, optionally filtered by namespace (e.g., 'ta', 'math', 'str', 'plot')",
		parameters: Type.Object({
			namespace: Type.Optional(
				Type.String({
					description:
						"Optional namespace filter (e.g., 'ta', 'math', 'str', 'plot')",
				}),
			),
		}),
		async execute(toolCallId, params, signal) {
			if (signal?.aborted) {
				return { content: [{ type: "text", text: "Cancelled." }], details: {} };
			}

			try {
				const { PineLanguageService, PineV6 } = await getServices();
				let functions: string[];

				if (params.namespace) {
					const members = PineV6.getNamespaceMembers(params.namespace);
					functions = members.functions.map((f: any) => f.name);
				} else {
					functions = PineLanguageService.getAllFunctions();
				}

				const output = {
					count: functions.length,
					namespace: params.namespace || "all",
					functions: functions.sort(),
				};

				// Truncate if too many functions
				const json = JSON.stringify(output, null, 2);
				const truncation = truncateHead(json, {
					maxLines: DEFAULT_MAX_LINES,
					maxBytes: DEFAULT_MAX_BYTES,
				});

				let text = truncation.content;
				if (truncation.truncated) {
					text += `\n\n[Truncated: ${truncation.outputLines}/${truncation.totalLines} lines shown.]`;
				}

				return {
					content: [{ type: "text", text }],
					details: { count: functions.length },
				};
			} catch (err: any) {
				return {
					content: [
						{
							type: "text",
							text: `Error: ${err.message}`,
						},
					],
					details: { error: err.message },
				};
			}
		},
		renderCall: (args, theme) =>
			new Text(
				theme.fg("toolTitle", theme.bold("pine_list_functions ")) +
					theme.fg("dim", args.namespace || "all"),
				0,
				0,
			),
		renderResult: (result, { isPartial }, theme) => {
			if (isPartial) return new Text(theme.fg("warning", "Listing…"), 0, 0);
			return new Text(
				theme.fg("success", `${result.details?.count || 0} functions`),
				0,
			);
		},
	});

	/**
	 * Format Pine Script code
	 */
	pi.registerTool({
		name: "pine_format",
		label: "Format Pine Script",
		description: "Format Pine Script code with proper indentation",
		parameters: Type.Object({
			code: Type.String({ description: "Pine Script code to format" }),
		}),
		async execute(toolCallId, params, signal) {
			if (signal?.aborted) {
				return { content: [{ type: "text", text: "Cancelled." }], details: {} };
			}

			try {
				const { PineLanguageService } = await getServices();
				const formatted = PineLanguageService.formatCode(params.code);

				return {
					content: [
						{
							type: "text",
							text: JSON.stringify({ formatted }, null, 2),
						},
					],
					details: { formatted: true },
				};
			} catch (err: any) {
				return {
					content: [
						{
							type: "text",
							text: `Error: ${err.message}`,
						},
					],
					details: { error: err.message },
				};
			}
		},
		renderCall: (args, theme) =>
			new Text(
				theme.fg("toolTitle", theme.bold("pine_format ")) +
					theme.fg("dim", `${args.code.split("\n").length} lines`),
				0,
			),
		renderResult: (result, { isPartial }, theme) => {
			if (isPartial) return new Text(theme.fg("warning", "Formatting…"), 0, 0);
			return new Text(theme.fg("success", "✓ Formatted"), 0, 0);
		},
	});

	/**
	 * Get all available namespaces
	 */
	pi.registerTool({
		name: "pine_list_namespaces",
		label: "List Pine Namespaces",
		description:
			"List all available Pine Script namespaces (ta, math, str, plot, input, etc.)",
		parameters: Type.Object({}),
		async execute(toolCallId, params, signal) {
			if (signal?.aborted) {
				return { content: [{ type: "text", text: "Cancelled." }], details: {} };
			}

			try {
				const { PineLanguageService } = await getServices();
				const namespaces = PineLanguageService.getAllNamespaces();

				return {
					content: [
						{
							type: "text",
							text: JSON.stringify(
								{
									count: namespaces.length,
									namespaces: namespaces.sort(),
								},
								null,
								2,
							),
						},
					],
					details: { count: namespaces.length },
				};
			} catch (err: any) {
				return {
					content: [
						{
							type: "text",
							text: `Error: ${err.message}`,
						},
					],
					details: { error: err.message },
				};
			}
		},
		renderCall: (_args, theme) =>
			new Text(
				theme.fg("toolTitle", theme.bold("pine_list_namespaces")),
				0,
				0,
			),
		renderResult: (result, { isPartial }, theme) => {
			if (isPartial) return new Text(theme.fg("warning", "Listing…"), 0, 0);
			return new Text(
				theme.fg("success", `${result.details?.count || 0} namespaces`),
				0,
			);
		},
	});

	// ========== COMMANDS ==========

	/**
	 * Command to configure the repo location
	 */
	pi.registerCommand("pine:configure", {
		description:
			"Configure the Pine Tools repository location for this extension",
		handler: async (args, ctx) => {
			const current = REPO_DIR;
			ctx.ui.notify(`Current repo location: ${current}`, "info");
		},
	});

	// ========== EVENT HOOKS ==========

	/**
	 * Optionally hook into context to provide Pine-specific context
	 */
	pi.on("before_agent_start", async (event, ctx) => {
		// Could inject Pine-specific system prompts here if needed
		return {};
	});

	// Register info about the extension
	console.log("[pine-tools] Extension loaded. Repo location:", REPO_DIR);
}
