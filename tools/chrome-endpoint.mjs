// Only trust the endpoint announced by the Chrome child we launched. Probing
// a fixed port can attach to somebody else's browser and authenticated profile.
export function waitForChromeEndpoint(chrome, timeoutMs = 60_000) {
  return new Promise((resolve, reject) => {
    let stderr = "";
    const finish = (error, endpoint) => {
      clearTimeout(timer);
      chrome.stderr.off("data", onData);
      chrome.off("exit", onExit);
      chrome.off("error", onError);
      // Continue draining the pipe after startup so logging cannot block Chrome.
      chrome.stderr.resume();
      error ? reject(error) : resolve(endpoint);
    };
    const onData = (chunk) => {
      stderr = (stderr + chunk.toString()).slice(-65_536);
      // Wait for a whole line; a WebSocket UUID may be split across chunks.
      const match = stderr.match(/DevTools listening on (ws:\/\/\S+)\r?\n/);
      if (match) finish(null, match[1]);
    };
    const onExit = (code, signal) => finish(new Error(
      `Chrome exited before announcing its endpoint (code ${code}, signal ${signal})\n${stderr.trim()}`,
    ));
    const onError = (error) => finish(new Error(`Could not start Chrome: ${error.message}`));
    const timer = setTimeout(() => finish(new Error(
      `Chrome never announced a debugging endpoint within ${timeoutMs}ms\n${stderr.trim()}`,
    )), timeoutMs);
    chrome.stderr.on("data", onData);
    chrome.once("exit", onExit);
    chrome.once("error", onError);
    if (chrome.exitCode != null || chrome.signalCode != null)
      onExit(chrome.exitCode, chrome.signalCode);
  });
}
