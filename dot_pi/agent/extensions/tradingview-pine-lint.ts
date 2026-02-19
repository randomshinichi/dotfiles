import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import {
  DEFAULT_MAX_BYTES,
  DEFAULT_MAX_LINES,
  truncateHead,
  formatSize,
} from "@mariozechner/pi-coding-agent";
import { Text } from "@mariozechner/pi-tui";
import { Type } from "@sinclair/typebox";
import { readFile } from "node:fs/promises";
import path from "node:path";

type RawDiagnostic = {
  start?: { line?: number; column?: number };
  end?: { line?: number; column?: number };
  message?: string;
};

type LintResponse = {
  result?: {
    errors2?: RawDiagnostic[];
    warnings2?: RawDiagnostic[];
    errors?: RawDiagnostic[];
    warnings?: RawDiagnostic[];
  };
  reason2?: {
    errors?: RawDiagnostic[];
    warnings?: RawDiagnostic[];
  };
};

type OutputDiagnostic = {
  resource: string;
  owner: string;
  severity: number;
  message: string;
  startLineNumber: number;
  startColumn: number;
  endLineNumber: number;
  endColumn: number;
  origin: string;
};

type LintParams = {
  path?: string;
  code?: string;
  owner?: string;
};

const PINE_FACADE_URL =
  "https://pine-facade.tradingview.com/pine-facade/translate_light?user_name=Guest&pine_id=00000000-0000-0000-0000-000000000000";

function toOutputDiagnostic(
  d: RawDiagnostic,
  severity: number,
  resource: string,
  owner: string,
): OutputDiagnostic {
  return {
    resource,
    owner,
    severity,
    message: d.message ?? "Unknown Pine diagnostic",
    startLineNumber: Math.max(1, d.start?.line ?? 1),
    startColumn: Math.max(1, d.start?.column ?? 1),
    endLineNumber: Math.max(1, d.end?.line ?? d.start?.line ?? 1),
    endColumn: Math.max(1, d.end?.column ?? d.start?.column ?? 1),
    origin: "extHost1",
  };
}

function collectDiagnostics(response: LintResponse, resource: string, owner: string): OutputDiagnostic[] {
  const errors2 = response.result?.errors2 ?? response.reason2?.errors ?? [];
  const warnings2 = response.result?.warnings2 ?? response.reason2?.warnings ?? [];
  const errors = response.result?.errors ?? [];
  const warnings = response.result?.warnings ?? [];

  const out: OutputDiagnostic[] = [];

  for (const d of errors2) out.push(toOutputDiagnostic(d, 8, resource, owner));
  for (const d of errors) out.push(toOutputDiagnostic(d, 8, resource, owner));
  for (const d of warnings2) out.push(toOutputDiagnostic(d, 4, resource, owner));
  for (const d of warnings) out.push(toOutputDiagnostic(d, 4, resource, owner));

  return out;
}

async function runLint(
  params: LintParams,
  signal?: AbortSignal,
  onUpdate?: (update: { content: { type: "text"; text: string }[] }) => void,
) {
  if (signal?.aborted) {
    return { content: [{ type: "text", text: "Cancelled." }], details: { cancelled: true } };
  }

  const owner = params.owner ?? "pine";
  const hasPath = typeof params.path === "string" && params.path.trim().length > 0;
  const hasCode = typeof params.code === "string" && params.code.trim().length > 0;

  if (!hasPath && !hasCode) {
    return {
      content: [{ type: "text", text: "Provide either `path` or `code`." }],
      details: { error: "missing_input" },
      isError: true,
    };
  }

  onUpdate?.({ content: [{ type: "text", text: "Preparing Pine source…" }] });

  const resource = hasPath ? path.resolve(params.path as string) : "inline://pine-script";
  const source = hasCode ? (params.code as string) : await readFile(resource, "utf8");

  if (signal?.aborted) {
    return { content: [{ type: "text", text: "Cancelled." }], details: { cancelled: true } };
  }

  onUpdate?.({ content: [{ type: "text", text: "Calling TradingView lint API…" }] });

  const formData = new URLSearchParams();
  formData.append("source", source || " ");

  const resp = await fetch(PINE_FACADE_URL, {
    method: "POST",
    headers: {
      Accept: "application/json",
      Referer: "https://www.tradingview.com/",
    },
    body: formData,
    signal,
  });

  if (!resp.ok) {
    return {
      content: [{ type: "text", text: `Lint request failed: HTTP ${resp.status} ${resp.statusText}` }],
      details: { error: `http_${resp.status}` },
      isError: true,
    };
  }

  const data = (await resp.json()) as LintResponse;
  const diagnostics = collectDiagnostics(data, resource, owner);

  const raw = diagnostics.length ? JSON.stringify(diagnostics, null, 2) : "[]\n\nNo diagnostics returned.";

  const truncation = truncateHead(raw, {
    maxLines: DEFAULT_MAX_LINES,
    maxBytes: DEFAULT_MAX_BYTES,
  });

  let text = truncation.content;
  if (truncation.truncated) {
    text += `\n\n[Truncated: ${truncation.outputLines}/${truncation.totalLines} lines, ${formatSize(
      truncation.outputBytes,
    )}/${formatSize(truncation.totalBytes)} shown.]`;
  }

  return {
    content: [{ type: "text", text }],
    details: {
      resource,
      owner,
      count: diagnostics.length,
      diagnostics,
      hasHistoryOperatorUDTError: diagnostics.some((d) =>
        d.message.includes("Cannot use the history-referencing operator on fields of user-defined types"),
      ),
    },
  };
}

export default function (pi: ExtensionAPI) {
  pi.registerTool({
    name: "tradingview_pine_lint",
    label: "TradingView Pine Lint",
    description:
      "Validate Pine Script through TradingView's pine-facade API and return VS Code style diagnostics.",
    parameters: Type.Object({
      path: Type.Optional(Type.String({ description: "Path to a .pine file to lint" })),
      code: Type.Optional(Type.String({ description: "Raw Pine code (use instead of path)" })),
      owner: Type.Optional(Type.String({ description: "Diagnostic owner label", default: "pine" })),
    }),
    async execute(_toolCallId, params, signal, onUpdate) {
      try {
        return await runLint(params, signal, onUpdate);
      } catch (err: any) {
        const msg = err?.message ?? String(err);
        return {
          content: [{ type: "text", text: `Lint failed: ${msg}` }],
          details: { error: msg },
          isError: true,
        };
      }
    },
    renderCall: (args, theme) => {
      const target = args.path ? path.resolve(args.path) : "inline code";
      return new Text(theme.fg("toolTitle", theme.bold("tradingview_pine_lint ")) + theme.fg("accent", target), 0, 0);
    },
    renderResult: (result, { isPartial }, theme) => {
      if (isPartial) return new Text(theme.fg("warning", "Linting…"), 0, 0);
      if (result.isError) return new Text(theme.fg("error", "✗ Lint failed"), 0, 0);

      const count = result.details?.count ?? 0;
      const special = result.details?.hasHistoryOperatorUDTError ? " (includes UDT history operator error)" : "";
      return new Text(theme.fg("success", `✓ ${count} diagnostics${special}`), 0, 0);
    },
  });

  pi.registerCommand("tvlint", {
    description: "Run TradingView Pine lint for a .pine file path",
    handler: async (args, ctx) => {
      const rawPath = (args ?? "").trim();
      if (!rawPath) {
        ctx.ui.notify("Usage: /tvlint <path-to-file.pine>", "warning");
        return;
      }

      try {
        ctx.ui.setStatus("tvlint", "Linting…");
        const result = await runLint({ path: rawPath, owner: "pine" });

        if (result.isError) {
          ctx.ui.notify(`tvlint failed: ${result.details?.error ?? "unknown error"}`, "error");
          return;
        }

        const count = result.details?.count ?? 0;
        const resource = result.details?.resource ?? path.resolve(rawPath);
        const hasSpecial = result.details?.hasHistoryOperatorUDTError ? " (includes UDT history operator error)" : "";
        ctx.ui.notify(`tvlint: ${count} diagnostics for ${resource}${hasSpecial}`, count > 0 ? "warning" : "info");
      } catch (err: any) {
        ctx.ui.notify(`tvlint failed: ${err?.message ?? String(err)}`, "error");
      } finally {
        ctx.ui.setStatus("tvlint", undefined);
      }
    },
  });
}
