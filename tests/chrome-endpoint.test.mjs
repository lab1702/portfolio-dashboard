import assert from "node:assert/strict";
import { EventEmitter } from "node:events";
import { PassThrough } from "node:stream";
import test from "node:test";
import { waitForChromeEndpoint } from "../tools/chrome-endpoint.mjs";

function child() {
  const chrome = new EventEmitter();
  chrome.stderr = new PassThrough();
  chrome.exitCode = null;
  chrome.signalCode = null;
  return chrome;
}

test("uses the owned child's complete endpoint, including split output", async () => {
  const chrome = child();
  const endpoint = waitForChromeEndpoint(chrome);
  chrome.stderr.write("startup log\nDevTools listening on ws://127.0.0.1:43127/devtools/browser/first-");
  chrome.stderr.write("second\r\n");
  assert.equal(await endpoint, "ws://127.0.0.1:43127/devtools/browser/first-second");
  assert.equal(chrome.listenerCount("exit"), 0);
});

test("a child that fails to bind cannot fall back to another browser", async () => {
  const chrome = child();
  const endpoint = waitForChromeEndpoint(chrome);
  chrome.stderr.write("bind failed: address already in use\n");
  chrome.emit("exit", 1, null);
  await assert.rejects(endpoint, /address already in use/);
});

test("reports spawn errors instead of waiting for a port", async () => {
  const chrome = child();
  const endpoint = waitForChromeEndpoint(chrome);
  chrome.emit("error", new Error("ENOENT"));
  await assert.rejects(endpoint, /Could not start Chrome: ENOENT/);
});

test("startup times out when the child never announces an endpoint", async () => {
  await assert.rejects(waitForChromeEndpoint(child(), 10), /never announced/);
});

test("handles a child that already exited", async () => {
  const chrome = child();
  chrome.exitCode = 1;
  await assert.rejects(waitForChromeEndpoint(chrome), /code 1/);
});
