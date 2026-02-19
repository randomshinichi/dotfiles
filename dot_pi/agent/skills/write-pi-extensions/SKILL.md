---
name: write-pi-extensions
description: Guide for creating pi extensions that register LLM-callable tools, user-facing commands, and event hooks. Use when writing custom extensions (.ts files) that add capabilities to the pi coding agent, including web search tools, file operations, custom TUI rendering, and state persistence.
---

# Pi Extensions & Tools

An **extension** is a `.ts` file pi loads at startup. A **tool** is a function the LLM can call. Extensions are the packaging; tools are one thing extensions can register. You write extensions; the LLM calls tools.

```
Extension (.ts file)           ← pi loads this automatically
  └─ pi.registerTool(...)      ← LLM-callable capability
  └─ pi.registerCommand(...)   ← user-facing /command
  └─ pi.on("tool_call", ...)   ← lifecycle event hook
```

---

## Extension locations

| Path | Scope |
|------|-------|
| `~/.pi/agent/extensions/searxng.ts` | Global — all projects |
| `.pi/extensions/searxng.ts` | Project-local |

Auto-discovered. No registration needed. Just drop the file in.

**Test without installing:**
```bash
pi -e ./my-extension.ts
```

**Hot-reload while pi is running:**
```
/reload
```

---

## Minimal extension skeleton

```typescript
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  // register tools, commands, event hooks here
}
```

---

## Registering a tool

```typescript
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { StringEnum } from "@mariozechner/pi-ai";
import { Type } from "@sinclair/typebox";

export default function (pi: ExtensionAPI) {
  pi.registerTool({
    name: "my_tool",           // identifier the LLM uses
    label: "My Tool",          // human-readable display name
    description: "What this tool does and when to use it.",
    parameters: Type.Object({
      text: Type.String({ description: "Some input" }),
      mode: StringEnum(["fast", "slow"] as const), // use StringEnum, NOT Type.Union/Literal
    }),
    async execute(toolCallId, params, signal, onUpdate, ctx) {
      return {
        content: [{ type: "text", text: `Result: ${params.text}` }], // sent to LLM
        details: { extra: "data" },                                   // for rendering only
      };
    },
  });
}
```

### Critical: use StringEnum for enums

`Type.Union([Type.Literal("a"), ...])` **breaks with Google models**. Always use:

```typescript
import { StringEnum } from "@mariozechner/pi-ai";

mode: StringEnum(["fast", "slow"] as const)
```

### Critical: truncate output

Tools that return large output will blow the context window. Always truncate:

```typescript
import {
  truncateHead,       // keep first N lines/bytes (files, search results)
  truncateTail,       // keep last N lines/bytes (logs, command output)
  DEFAULT_MAX_BYTES,  // 50 KB
  DEFAULT_MAX_LINES,  // 2000 lines
  formatSize,
} from "@mariozechner/pi-coding-agent";

const truncation = truncateHead(rawOutput, {
  maxLines: DEFAULT_MAX_LINES,
  maxBytes: DEFAULT_MAX_BYTES,
});

let text = truncation.content;
if (truncation.truncated) {
  text += `\n\n[Truncated: ${truncation.outputLines}/${truncation.totalLines} lines shown.]`;
}
```

### Streaming progress during execution

```typescript
async execute(toolCallId, params, signal, onUpdate, ctx) {
  onUpdate?.({ content: [{ type: "text", text: "Step 1 done…" }] });
  // ... more work ...
  return { content: [{ type: "text", text: "Final result" }], details: {} };
}
```

### Checking for cancellation

```typescript
async execute(toolCallId, params, signal, onUpdate, ctx) {
  if (signal?.aborted) {
    return { content: [{ type: "text", text: "Cancelled." }], details: {} };
  }
  // ...
}
```

---

## Registering a command

Commands are user-facing (`/mycommand`). They don't go into the LLM context.

```typescript
pi.registerCommand("stats", {
  description: "Show session statistics",
  handler: async (args, ctx) => {
    const count = ctx.sessionManager.getEntries().length;
    ctx.ui.notify(`${count} entries`, "info");
  },
});
```

With argument autocomplete:

```typescript
pi.registerCommand("deploy", {
  description: "Deploy to an environment",
  getArgumentCompletions: (prefix) => {
    return ["dev", "staging", "prod"]
      .filter(e => e.startsWith(prefix))
      .map(e => ({ value: e, label: e }));
  },
  handler: async (args, ctx) => {
    ctx.ui.notify(`Deploying to ${args}`, "info");
  },
});
```

---

## Event hooks

Hook into the agent lifecycle without the LLM knowing.

```typescript
// Before every LLM call — rewrite/prune the message array
pi.on("context", async (event, ctx) => {
  const filtered = event.messages.filter(m => !shouldPrune(m));
  return { messages: filtered };
});

// Before a tool executes — can block it
pi.on("tool_call", async (event, ctx) => {
  if (event.toolName === "bash" && event.input.command?.includes("rm -rf")) {
    const ok = await ctx.ui.confirm("Dangerous!", "Allow rm -rf?");
    if (!ok) return { block: true, reason: "Blocked by user" };
  }
});

// After a tool executes — can modify the result
pi.on("tool_result", async (event, ctx) => {
  // return { content: [...], details: {...}, isError: false } to override
});

// After user submits a prompt, before agent loop — inject context
pi.on("before_agent_start", async (event, ctx) => {
  return {
    systemPrompt: event.systemPrompt + "\n\nExtra instructions.",
    message: {
      customType: "my-ext",
      content: "Injected context for this turn.",
      display: true,
    },
  };
});

// Session lifecycle
pi.on("session_start", async (_event, ctx) => { /* restore state */ });
pi.on("session_shutdown", async (_event, ctx) => { /* cleanup */ });
```

### Typed tool_call events

```typescript
import { isToolCallEventType } from "@mariozechner/pi-coding-agent";

pi.on("tool_call", async (event, ctx) => {
  if (isToolCallEventType("bash", event)) {
    // event.input is { command: string; timeout?: number }
    console.log(event.input.command);
  }
  if (isToolCallEventType("read", event)) {
    // event.input is { path: string; offset?: number; limit?: number }
  }
});
```

---

## User interaction (ctx.ui)

```typescript
const choice  = await ctx.ui.select("Pick one:", ["A", "B", "C"]);
const ok      = await ctx.ui.confirm("Delete?", "Cannot be undone");
const name    = await ctx.ui.input("Name:", "placeholder");
const text    = await ctx.ui.editor("Edit:", "prefilled text");

ctx.ui.notify("Done!", "info");        // "info" | "warning" | "error"
ctx.ui.setStatus("my-ext", "Running…"); // footer status
ctx.ui.setStatus("my-ext", undefined);  // clear it
```

With timeout:

```typescript
const ok = await ctx.ui.confirm("Continue?", "Auto-cancels in 5s", { timeout: 5000 });
```

---

## Custom TUI rendering for tools

```typescript
import { Text } from "@mariozechner/pi-tui";

pi.registerTool({
  // ...
  renderCall(args, theme) {
    // shown before/during execution
    let text = theme.fg("toolTitle", theme.bold("my_tool "));
    text += theme.fg("accent", args.text);
    return new Text(text, 0, 0); // always 0,0 padding — Box handles it
  },

  renderResult(result, { expanded, isPartial }, theme) {
    if (isPartial) return new Text(theme.fg("warning", "Working…"), 0, 0);
    if (result.details?.error) return new Text(theme.fg("error", `✗ ${result.details.error}`), 0, 0);

    let text = theme.fg("success", "✓ Done");
    if (expanded) {
      // show detail when user presses Ctrl+O
      text += "\n" + theme.fg("dim", result.content[0]?.text ?? "");
    }
    return new Text(text, 0, 0);
  },
});
```

---

## State persistence

State stored in tool `details` survives session branching/forking automatically.
For extension-private state (not sent to LLM), use `pi.appendEntry()`:

```typescript
pi.appendEntry("my-state", { count: 42 });

pi.on("session_start", async (_event, ctx) => {
  for (const entry of ctx.sessionManager.getBranch()) {
    if (entry.type === "custom" && entry.customType === "my-state") {
      // restore from entry.data
    }
  }
});
```

---

## Multi-file extension (with npm dependencies)

```
~/.pi/agent/extensions/my-extension/
├── package.json
├── package-lock.json
├── node_modules/
└── index.ts        ← entry point, exports default function
```

```json
// package.json
{
  "name": "my-extension",
  "dependencies": { "some-package": "^1.0.0" },
  "pi": { "extensions": ["./index.ts"] }
}
```

Run `npm install` in the directory. Imports from `node_modules/` are resolved automatically.

---

## Available imports

| Package | What you get |
|---------|-------------|
| `@mariozechner/pi-coding-agent` | `ExtensionAPI`, `ExtensionContext`, event types, truncation utils, `isToolCallEventType` |
| `@sinclair/typebox` | `Type` — for parameter schemas |
| `@mariozechner/pi-ai` | `StringEnum` — for enum params (Google-compatible) |
| `@mariozechner/pi-tui` | `Text`, `Markdown`, and other TUI components for custom rendering |
| Node.js built-ins | `node:fs`, `node:path`, etc. — available without install |

---

## Real-world example: SearXNG web search

A complete extension that adds a `search` tool pointing at a local SearXNG instance.

```typescript
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { DEFAULT_MAX_BYTES, DEFAULT_MAX_LINES, formatSize, truncateHead } from "@mariozechner/pi-coding-agent";
import { StringEnum } from "@mariozechner/pi-ai";
import { Text } from "@mariozechner/pi-tui";
import { Type } from "@sinclair/typebox";

const SEARXNG_BASE_URL = "http://localhost:8888";
const MAX_RESULTS = 10;

export default function (pi: ExtensionAPI) {
  pi.registerTool({
    name: "search",
    label: "Web Search",
    description: `Search the web via SearXNG (${SEARXNG_BASE_URL}). Returns titles, URLs, snippets.`,
    parameters: Type.Object({
      query: Type.String({ description: "Search query" }),
      categories: Type.Optional(Type.String({ description: "e.g. general, news, science" })),
      time_range: Type.Optional(StringEnum(["day", "month", "year"] as const)),
      language: Type.Optional(Type.String({ description: "e.g. en, de" })),
    }),
    async execute(_id, params) {
      const url = new URL("/search", SEARXNG_BASE_URL);
      url.searchParams.set("q", params.query);
      url.searchParams.set("format", "json");
      if (params.categories) url.searchParams.set("categories", params.categories);
      if (params.time_range) url.searchParams.set("time_range", params.time_range);
      if (params.language) url.searchParams.set("language", params.language);

      let data: any;
      try {
        const resp = await fetch(url.toString(), { headers: { Accept: "application/json" } });
        if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
        data = await resp.json();
      } catch (err: any) {
        return { content: [{ type: "text", text: `Search failed: ${err.message}` }], details: { error: err.message } };
      }

      const results = (data.results ?? []).slice(0, MAX_RESULTS);
      if (!results.length) return { content: [{ type: "text", text: "No results found." }], details: { returned: 0 } };

      const raw = results.map((r: any, i: number) =>
        `${i + 1}. **${r.title ?? "(no title)"}**\n   ${r.url}\n   ${r.content ?? ""}`
      ).join("\n\n");

      const truncation = truncateHead(raw, { maxLines: DEFAULT_MAX_LINES, maxBytes: DEFAULT_MAX_BYTES });
      const text = truncation.truncated
        ? truncation.content + `\n\n[Truncated: ${truncation.outputLines}/${truncation.totalLines} lines shown.]`
        : truncation.content;

      return { content: [{ type: "text", text }], details: { returned: results.length } };
    },
    renderCall: (args, theme) =>
      new Text(theme.fg("toolTitle", theme.bold("search ")) + theme.fg("accent", `"${args.query}"`), 0, 0),
    renderResult: (result, { isPartial }, theme) =>
      isPartial
        ? new Text(theme.fg("warning", "Searching…"), 0, 0)
        : new Text(theme.fg("success", `${result.details?.returned ?? 0} results`), 0, 0),
  });
}
```
