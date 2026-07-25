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

// Two modes. The capture mode needs the live Shiny app, because a screenshot of
// unpainted cards is worthless. The check mode does not: the colour audit only
// needs the markup and the stylesheet, both of which `quarto render` puts in the
// static html. That matters because the audit is the only thing in this repo
// that resolves the compiled cascade, and a check nobody can run without
// standing up a server and hitting Yahoo Finance is a check nobody runs.
const CHECK_ONLY = process.argv[2] === "--check";
const URL_ = CHECK_ONLY ? process.argv[3] : process.argv[2];
const OUT = CHECK_ONLY ? null : process.argv[3];
if (!URL_ || (!CHECK_ONLY && !OUT)) {
  console.error("usage: node tools/capture-screenshot.mjs <url> <out.png>");
  console.error("       node tools/capture-screenshot.mjs --check <url>");
  console.error("  e.g. node tools/capture-screenshot.mjs --check " +
                "file:///$(pwd)/portfolio_dashboard.html");
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

  // Static html has the markup and the stylesheet but no Shiny to fill the
  // outputs, so waiting for them to paint would time out for nothing. The
  // colour audit does not need them; the capture does, which is why capture
  // mode still waits.
  if (CHECK_ONLY) {
    console.log("check mode: skipping readiness (outputs are not painted)");
  } else {
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
  }

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
  // Background AND text. Checking only the background is half a contrast pair,
  // and a regression that reverted the ink while leaving the fill would have
  // passed — which was a real gap, not a hypothetical one.
  const WANT = [
    { sel: ".bslib-value-box.bg-primary", bg: "rgb(31, 95, 174)",   fg: "rgb(255, 255, 255)" },
    { sel: ".bslib-value-box.bg-light",   bg: "rgb(15, 15, 15)",    fg: "rgb(237, 237, 237)" },
    { sel: ".sidebar code",               bg: "rgb(26, 26, 26)",    fg: "rgb(237, 237, 237)" },
    // Transient UI. None of it can ever appear in a screenshot, which is
    // exactly why it went unthemed: withProgress() paints a notification on
    // every backtest, over the slowest part of the app, and Shiny's own default
    // for it is #e8e8e8 on #333.
    { sel: ".shiny-notification",         bg: "rgb(26, 26, 26)",    fg: "rgb(237, 237, 237)" },
    { sel: ".shiny-notification-close",   fg: "rgb(133, 133, 133)" },
    { sel: ".shiny-progress-notification .progress",     bg: "rgb(58, 58, 58)"  },
    { sel: ".shiny-progress-notification .progress-bar", bg: "rgb(79, 155, 240)" },
  ];

  // The transient elements are not in the DOM until Shiny creates them, so the
  // audit creates them itself. This proves the stylesheet, not Shiny — the
  // markup below mirrors what shiny.js builds for a progress notification, and
  // if Shiny's own class names ever drift from it, the selectors here go on
  // matching this fixture while the real thing goes unstyled. The zero-match
  // rule below is what makes that visible: delete a class here and the audit
  // reports the selector matching nothing rather than quietly passing.
  await evaluate(`(() => {
    const panel = document.createElement('div');
    panel.id = 'shiny-notification-panel';
    panel.innerHTML =
      '<div class="shiny-notification">' +
        '<div class="shiny-notification-content">' +
          '<div class="shiny-notification-content-text">' +
            '<div class="shiny-progress-notification">' +
              '<div class="progress"><div class="progress-bar" style="width:40%"></div></div>' +
              '<div class="progress-text">' +
                '<span class="progress-message">Downloading prices</span>' +
                '<span class="progress-detail">VOO</span>' +
              '</div>' +
            '</div>' +
          '</div>' +
        '</div>' +
        '<div class="shiny-notification-close">&times;</div>' +
      '</div>';
    document.body.appendChild(panel);
    return true;
  })()`);

  const colorAudit = await evaluate(`(() => {
    const want = ${JSON.stringify(WANT)};
    const out = [];
    for (const { sel, bg, fg } of want) {
      const els = document.querySelectorAll(sel);
      if (els.length === 0) {
        out.push({ sel, got: "(selector matched no elements)", want: bg || fg });
        continue;
      }
      els.forEach((el) => {
        const s = getComputedStyle(el);
        if (bg && s.backgroundColor !== bg)
          out.push({ sel, prop: "background", got: s.backgroundColor, want: bg });
        if (fg && s.color !== fg)
          out.push({ sel, prop: "color", got: s.color, want: fg });
      });
    }
    return out;
  })()`);

  // ── Capture ───────────────────────────────────────────────────────────────
  if (!CHECK_ONLY) {
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
  }

  if (CHECK_ONLY) {
    // The overflow audit measures scroll extents against painted content, and
    // in check mode there is none, so its answer would be meaningless rather
    // than merely uninteresting.
    console.log("overflow audit: skipped (needs painted content)");
  } else if (overflow.length) {
    console.error("cards clipping content:", JSON.stringify(overflow, null, 2));
    console.error("image written, but fix the clipping before committing it");
    exitCode = 2;
  } else {
    console.log("overflow audit: no card is clipping content");
  }

  if (colorAudit.length) {
    console.error("colours lost the cascade:", JSON.stringify(colorAudit, null, 2));
    console.error(CHECK_ONLY
      ? "run this against the served app too — the static html omits shiny.min.css,"
        + " which loads after the theme and can win on source order"
      : "image written, but fix the colour mismatch before committing it");
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
