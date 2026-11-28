"use strict";

const terminal = new window.Terminal({
  cursorBlink: true,
  fontSize: 14,
  scrollback: 0,
  allowProposedApi: true,
  theme: { background: "#1a1a1a" }
});
const fit = new window.FitAddon.FitAddon();
const container = document.getElementById("editor");
const status = document.getElementById("connection");
const specialKeys = new Set([
  "Tab", "Enter", "Backspace", "Escape", "ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight",
  "Home", "End", "Insert", "Delete", "PageUp", "PageDown", "F1", "F2", "F3", "F4"
]);
let socket;
let reconnectDelay = 250;
let noticeTimer;
let paintedSize;
let pendingFrame;
let writing = false;
const maxTextLength = 1024 * 1024;

terminal.loadAddon(fit);
terminal.loadAddon(new window.Unicode11Addon.Unicode11Addon());
terminal.unicode.activeVersion = "11";
terminal.open(container);
terminal.focus();

function setStatus(message, transient = false) {
  clearTimeout(noticeTimer);
  status.textContent = message;
  if (transient) noticeTimer = setTimeout(() => { status.textContent = ""; }, 4000);
}

function send(packet) {
  if (socket?.readyState === WebSocket.OPEN) socket.send(JSON.stringify(packet));
}

function fitTerminal() {
  const size = fit.proposeDimensions();
  if (!size) return;
  const rows = Math.max(2, Math.min(300, size.rows));
  const cols = Math.max(2, Math.min(500, size.cols));
  if (rows === terminal.rows && cols === terminal.cols) return;
  paintedSize = undefined;
  terminal.resize(cols, rows);
  send(["resize", rows, cols]);
}

function drawFrame() {
  if (writing || !pendingFrame) return;
  const [, rows, cols, ansi] = pendingFrame;
  pendingFrame = undefined;
  if (rows !== terminal.rows || cols !== terminal.cols) return;

  const size = `${rows}x${cols}`;
  const connection = socket;
  writing = true;
  const clear = size === paintedSize ? "" : "\x1b[2J";
  terminal.write("\x1b[?7l" + clear + ansi, () => {
    writing = false;
    if (connection === socket && socket.readyState === WebSocket.OPEN &&
        rows === terminal.rows && cols === terminal.cols) {
      if (paintedSize === undefined) setStatus("");
      paintedSize = size;
    }
    drawFrame();
  });
}

function connect() {
  const url = new URL("/ws", location.href);
  url.protocol = location.protocol === "https:" ? "wss:" : "ws:";
  const connection = new WebSocket(url);
  socket = connection;
  connection.addEventListener("open", () => {
    fitTerminal();
    send(["resize", terminal.rows, terminal.cols]);
  });
  connection.addEventListener("message", event => {
    const packet = JSON.parse(event.data);
    switch (packet[0]) {
      case "frame":
        reconnectDelay = 250;
        pendingFrame = packet;
        drawFrame();
        break;
      case "notice":
        setStatus(packet[1], true);
        break;
      case "clipboard":
        Promise.resolve().then(() => navigator.clipboard.writeText(packet[1])).catch(() => {
          if (connection === socket && socket.readyState === WebSocket.OPEN) {
            setStatus("Browser clipboard unavailable. Yank with Ctrl+Y.", true);
          }
        });
        break;
    }
  });
  connection.addEventListener("close", () => {
    paintedSize = undefined;
    pendingFrame = undefined;
    setStatus("Disconnected. Reconnecting… Typing is paused.");
    setTimeout(connect, reconnectDelay);
    reconnectDelay = Math.min(reconnectDelay * 2, 3000);
  });
}

terminal.attachCustomKeyEventHandler(event => {
  const { key, ctrlKey, altKey, metaKey, shiftKey } = event;
  if (event.isComposing || event.getModifierState("AltGraph") || key === "Dead") return true;

  // Leave native copy/paste shortcuts to the browser. Ctrl+V is page-down in Nesmacs.
  const nativeClipboard = (ctrlKey && shiftKey && /^[cv]$/i.test(key)) ||
    (shiftKey && key === "Insert") || (ctrlKey && key.toLowerCase() === "c" && terminal.hasSelection());
  if (metaKey || nativeClipboard) return false;

  let packet;
  if (key.length === 1 && (ctrlKey || altKey)) {
    const modifier = ctrlKey ? (altKey ? "meta-ctrl" : "ctrl") : "meta";
    const character = ctrlKey && /^[a-z]$/i.test(key) ? key.toLowerCase() : key;
    packet = ["key", modifier, character];
  } else if (specialKeys.has(key) && !ctrlKey && !altKey) {
    packet = ["key", "special", key];
  }

  if (!packet) return true;
  if (event.type === "keydown") {
    event.preventDefault();
    send(packet);
  }
  return false;
});

terminal.onData(data => {
  // xterm emits committed IME text here as well as printable keystrokes.
  if (data && !/[\x00-\x1f\x7f]/.test(data)) send(["text", data]);
});

container.addEventListener("paste", event => {
  const text = event.clipboardData?.getData("text/plain");
  if (!text) return;
  event.preventDefault();
  event.stopPropagation();
  if (text.length > maxTextLength) {
    setStatus("Paste is too large. Paste it in smaller blocks.", true);
  } else {
    send(["paste", text]);
  }
}, { capture: true });

function mouseCell(event) {
  const screen = container.querySelector(".xterm-screen");
  if (!paintedSize || socket?.readyState !== WebSocket.OPEN || !screen) return;
  const { left, top, width, height } = screen.getBoundingClientRect();
  const row = Math.floor((event.clientY - top) * terminal.rows / height);
  const col = Math.floor((event.clientX - left) * terminal.cols / width);
  if (row >= 0 && row < terminal.rows && col >= 0 && col < terminal.cols) return [row, col];
}

container.addEventListener("wheel", event => {
  const cell = mouseCell(event);
  if (!cell || !event.deltaY || event.ctrlKey) return;
  event.preventDefault();
  event.stopPropagation();
  send(["mouse", event.deltaY < 0 ? "up" : "down", ...cell]);
}, { capture: true, passive: false });

container.addEventListener("click", event => {
  if (event.button !== 0 || terminal.hasSelection()) return;
  const cell = mouseCell(event);
  if (cell) send(["mouse", "click", ...cell]);
});

new ResizeObserver(fitTerminal).observe(container);
fitTerminal();
connect();
