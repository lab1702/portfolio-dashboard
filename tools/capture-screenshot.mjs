// tools/capture-screenshot.mjs — regenerate images/dashboard.png for the README.
//
//   quarto serve portfolio_dashboard.qmd --port 4455       # one terminal
//   node tools/capture-screenshot.mjs http://127.0.0.1:4455/ images/dashboard.png
//
// The README image is not produced by any build step, which is exactly why it
// goes stale silently whenever the layout or a visible caption changes. This
// script exists so retaking it is one command rather than a careful manual pass.
//
// No dependencies, by design: Node 18+ has a global WebSocket, so this talks the
// Chrome DevTools Protocol directly. puppeteer/webshot2/chromote would each be a
// download, and a screenshot tool that needs its own install is a screenshot
// tool nobody runs.

import { spawn } from "node:child_process";
import { existsSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const URL_ = process.argv[2];
const OUT = process.argv[3];
if (!URL_ || !OUT) {
  console.error("usage: node tools/capture-screenshot.mjs <url> <out.png>");
  process.exit(64);
}

// 1600x1150 at device scale factor 2. Both halves matter: the viewport decides
// the layout (the sidebar plus three columns need this width to avoid bslib
// collapsing the value boxes), and the scale factor decides the pixel density.
// Changing either changes the committed image's dimensions, so the README image
// would visibly jump size between commits.
const WIDTH = 1600, HEIGHT = 1150, SCALE = 2;
const CDP_PORT = Number(process.env.CDP_PORT || 9222);
const READY_TIMEOUT_MS = 180_000;

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function findChrome() {
  if (process.env.CHROME_PATH) return process.env.CHROME_PATH;
  const candidates = [
    "C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe",
    "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe",
    join(process.env.LOCALAPPDATA || "", "Google\\Chrome\\Application\\chrome.exe"),
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/usr/bin/google-chrome",
    "/usr/bin/chromium",
  ];
  const hit = candidates.find((p) => p && existsSync(p));
  if (!hit) {
    console.error("Chrome not found — set CHROME_PATH to the executable.");
    process.exit(69);
  }
  return hit;
}

// A throwaway profile, never the user's: this Chrome runs with remote debugging
// wide open, and it must not touch real cookies, sessions or extensions.
const profileDir = mkdtempSync(join(tmpdir(), "dashboard-shot-"));
const chrome = spawn(findChrome(), [
  "--headless=new",
  "--disable-gpu",
  `--remote-debugging-port=${CDP_PORT}`,
  `--user-data-dir=${profileDir}`,
  "--no-first-run",
  "--no-default-browser-check",
  "--hide-scrollbars",   // a scrollbar in the capture reads as a design element
  "about:blank",
], { stdio: "ignore" });

let exitCode = 0;

// An exit handler cannot await, and Windows holds handles on the profile for a
// moment after the process dies — a single rmSync there fails and leaks a temp
// directory on every run. Atomics.wait gives a synchronous pause to retry on.
const sleepSync = (ms) => Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, ms);

const cleanup = () => {
  try { chrome.kill("SIGKILL"); } catch {}
  for (let i = 0; i < 15; i++) {
    try { rmSync(profileDir, { recursive: true, force: true }); return; }
    catch { sleepSync(200); }
  }
  console.error(`note: could not remove temp profile ${profileDir}`);
};
process.on("exit", cleanup);

try {
  // ── Connect ───────────────────────────────────────────────────────────────
  let version = null;
  for (let i = 0; i < 30 && !version; i++) {
    try {
      version = await (await fetch(`http://127.0.0.1:${CDP_PORT}/json/version`)).json();
    } catch { await sleep(500); }
  }
  if (!version) throw new Error(`Chrome never opened a CDP port on ${CDP_PORT}`);

  const ws = new WebSocket(version.webSocketDebuggerUrl);
  await new Promise((res, rej) => { ws.onopen = res; ws.onerror = rej; });

  let nextId = 1;
  const pending = new Map();
  ws.onmessage = (e) => {
    const m = JSON.parse(e.data);
    if (m.id && pending.has(m.id)) {
      const { res, rej } = pending.get(m.id);
      pending.delete(m.id);
      m.error ? rej(new Error(JSON.stringify(m.error))) : res(m.result);
    }
  };
  const send = (method, params = {}, sessionId) =>
    new Promise((res, rej) => {
      const id = nextId++;
      pending.set(id, { res, rej });
      ws.send(JSON.stringify({ id, method, params, sessionId }));
    });

  const { targetId } = await send("Target.createTarget", { url: "about:blank" });
  const { sessionId } = await send("Target.attachToTarget", { targetId, flatten: true });
  const call = (method, params) => send(method, params, sessionId);

  await call("Page.enable");
  await call("Runtime.enable");

  // Set the metrics override *before* navigating. Applied after, the first
  // render happens at the wrong size and Shiny may not re-render the plots;
  // applied here, --window-size becomes irrelevant and the output is exactly
  // WIDTH*SCALE x HEIGHT*SCALE whatever the OS does with a headless window.
  await call("Emulation.setDeviceMetricsOverride", {
    width: WIDTH, height: HEIGHT, deviceScaleFactor: SCALE, mobile: false,
  });
  await call("Page.navigate", { url: URL_ });

  const evaluate = async (expression) => {
    const r = await call("Runtime.evaluate", {
      expression, returnByValue: true, awaitPromise: true,
    });
    if (r.exceptionDetails) throw new Error(JSON.stringify(r.exceptionDetails));
    return r.result.value;
  };

  // ── Wait for painted content, not for load ────────────────────────────────
  // Shiny renders plots and tables over a websocket well after the load event,
  // so anything that fires on load captures a grid of empty cards. Every output
  // the image is supposed to show gets its own check.
  const READY = `(() => {
    const img = (sel) => {
      const el = document.querySelector(sel + ' img');
      return !!el && el.naturalWidth > 50;
    };
    const rows = (sel, n) => document.querySelectorAll(sel).length >= n;
    const txt = (sel) => {
      const el = document.querySelector(sel);
      return !!el && el.textContent.trim().length > 0;
    };
    return {
      perf: img('#perf_chart'),
      fund: img('#fund_chart'),
      calendar: rows('#calendar_table td.cal-cell', 12),
      stats: rows('#stats_table table tbody tr', 3),
      drawdowns: rows('#dd_table table tbody tr', 3),
      growth: txt('#vb_growth'),
      period: txt('#vb_period'),
      caption: txt('#perf_caption'),
    };
  })()`;

  let ready = null;
  const deadline = Date.now() + READY_TIMEOUT_MS;
  while (Date.now() < deadline) {
    try {
      ready = await evaluate(READY);
      if (ready && Object.values(ready).every(Boolean)) break;
    } catch { /* page still navigating */ }
    await sleep(1000);
  }
  if (!ready || !Object.values(ready).every(Boolean)) {
    console.error("readiness:", JSON.stringify(ready));
    throw new Error("content never painted — refusing to capture empty cards");
  }
  console.log("readiness: all outputs painted");

  // Fonts and plot antialiasing settle after the data lands.
  await evaluate("document.fonts.ready.then(() => true)");
  await sleep(2500);

  // ── Audit clipping in the same pass ───────────────────────────────────────
  // A clipped column hides behind a scrollbar and looks complete in a
  // screenshot, so measuring beats eyeballing. This check is what caught three
  // cards silently truncating data, and it is worth re-running after any
  // type-size or letter-spacing change.
  const overflow = await evaluate(`(() => {
    const out = [];
    document.querySelectorAll('.card-body').forEach((el) => {
      const dx = el.scrollWidth - el.clientWidth;
      const dy = el.scrollHeight - el.clientHeight;
      if (dx > 1 || dy > 1) {
        const card = el.closest('.card, .bslib-value-box');
        const title = card && card.querySelector('.card-header, .value-box-title');
        out.push({ card: title ? title.textContent.trim() : '(untitled)',
                   overflowX: dx, overflowY: dy });
      }
    });
    return out;
  })()`);

  // ── Audit computed colours in the same pass ───────────────────────────────
  // The suite validates colour *values* exhaustively (test-palette.R) and
  // rendered *outcomes* not at all — which is how the panel titles, and then
  // the sidebar's inline-code pill, shipped looking set in source and losing
  // the cascade in the browser. A selector that matches zero elements is that
  // same failure mode wearing a different face — the page changed shape and
  // nothing complained — so it counts as a mismatch, not a silent pass.
  const WANT_BG = {
    ".bslib-value-box.bg-primary": "rgb(31, 95, 174)", // $viz-accent-fill
    ".bslib-value-box.bg-light":   "rgb(15, 15, 15)",  // $viz-surface
    ".sidebar code":               "rgb(26, 26, 26)",  // $code-bg
  };
  const colorAudit = await evaluate(`(() => {
    const want = ${JSON.stringify(WANT_BG)};
    const out = [];
    for (const [sel, bg] of Object.entries(want)) {
      const els = document.querySelectorAll(sel);
      if (els.length === 0) {
        out.push({ sel, got: "(selector matched no elements)", want: bg });
        continue;
      }
      els.forEach((el) => {
        const got = getComputedStyle(el).backgroundColor;
        if (got !== bg) out.push({ sel, got, want: bg });
      });
    }
    return out;
  })()`);

  // ── Capture ───────────────────────────────────────────────────────────────
  const shot = await call("Page.captureScreenshot", { format: "png" });
  const buf = Buffer.from(shot.data, "base64");
  writeFileSync(OUT, buf);

  // Read the dimensions back out of the PNG header rather than trusting that
  // the emulation override took: a silently-unapplied override produces a
  // plausible-looking image at the wrong size.
  const w = buf.readUInt32BE(16), h = buf.readUInt32BE(20);
  console.log(`wrote ${OUT}  ${w}x${h}  ${buf.length} bytes`);
  if (w !== WIDTH * SCALE || h !== HEIGHT * SCALE)
    throw new Error(`expected ${WIDTH * SCALE}x${HEIGHT * SCALE}, got ${w}x${h}`);

  if (overflow.length) {
    console.error("cards clipping content:", JSON.stringify(overflow, null, 2));
    console.error("image written, but fix the clipping before committing it");
    exitCode = 2;
  } else {
    console.log("overflow audit: no card is clipping content");
  }

  if (colorAudit.length) {
    console.error("colours lost the cascade:", JSON.stringify(colorAudit, null, 2));
    console.error("image written, but fix the colour mismatch before committing it");
    exitCode = 2;
  } else {
    console.log("colour audit: every computed background matches theme.scss");
  }
  ws.close();
} catch (err) {
  console.error(`FAILED: ${err.message}`);
  exitCode = 1;
}

process.exit(exitCode);
