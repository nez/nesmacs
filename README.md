# nesmacs

**Emacs' editing model, rebuilt in Clojure — and rewritable while it runs.**

[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
![Clojure](https://img.shields.io/badge/Clojure-native-63b132?logo=clojure&logoColor=white)
![Status](https://img.shields.io/badge/status-pre--1.0-orange)

Buffers, windows, key prefixes, mark, kill ring, modes and hooks — on a small
Clojure core. No Elisp runtime and no plugin boundary: configuration, commands
and fontification are Clojure in the same process as the editor, so the editor
is something you can inspect, change and debug while it is running.

<img src="docs/nesmacs.svg" alt="nesmacs editing a Clojure buffer, rendered by nesmacs.view" width="691">

<p align="center"><sub>Rendered by <code>nesmacs.view</code> from a real editor state — no mockup.</sub></p>

## Install

```sh
curl -fsSL https://raw.githubusercontent.com/nez/nesmacs/main/install.sh | sh
```

Needs Java 17+, `tmux` 3.0+, `git` and `ripgrep` on `PATH`. The script checks
them and installs nothing system-wide; it puts `nesmacs.jar` and a launcher
under `~/.local` (override with `--prefix DIR`, or pin a release with
`--version TAG`) and prints the `PATH` line if it is missing. `--uninstall`
removes the jar and launcher.

`C-x C-c` exits; it asks to save each modified buffer. First launch creates
`~/.nesmacs/` for config, session and the crash journal. If you have an old
`~/.xmas/`, the installer renames it once.

From a checkout, `./install.sh` uses `target/nesmacs.jar` or builds it with the
Clojure CLI. `make setup` is the contributor bootstrap (Clojure CLI, linter,
dev dependencies). `make run` rebuilds the AOT jar only when sources changed and
runs it (~1.5 s); `./nesmacs` runs the existing jar. `make dev` loads source
instead, so it starts in ~4 s but picks up edits without a restart.

Your fingers already work: `C-f C-b C-n C-p C-a C-e`, `C-s`, `M-%`, `C-x C-f`,
`C-x C-s`, `C-x 2`, `C-x o`, `M-x`, `M-:`. Beyond those: `C-x g` git status,
`C-x p g` ripgrep the project, `M-.` definition, `C-c h` hover, `C-c i`
complete symbol, `C-x r` registers and rectangles, and `C-c t` opens a
terminal.

Real terminals are core: `C-c t` runs `$SHELL` in a tmux session, so the shell
(outside the editor) survives an editor exit or crash and reattaches next start.
`C-c C-c` interrupts, `C-c C-k` kills the buffer and session, `C-c C-r`
restarts, `C-c M-x` reaches `M-x`, and `C-c C-x` opens the normal `C-x` map
from inside a terminal. Clicking a terminal window focuses it; clicks are not
forwarded to the shell, and the pane's cursor is not selectable text.

## The part that is different

The editor is one atom. Every command is a pure function from that state to the
next one. That is not an implementation detail — it is the feature:

```clojure
;; nREPL is on :7888. Connect from CIDER/Calva, or from another terminal:
user=> (require '[nesmacs.ed :as ed] '[nesmacs.cmd :as cmd])
user=> (str (:text (cmd/cur @ed/editor)))
"(ns nesmacs.hello\n  (:require [nesmacs.api :as api]))\n..."
user=> (swap! ed/editor cmd/edit 0 0 ";; live from the REPL\n")
;; the buffer on screen is now different
```

Or skip the socket and use `M-:`, which evaluates against the live editor:

```clojure
M-: (api/bind! "C-c g" (fn [s] (assoc s :msg "hi")))
```

Because the state is a value, the same command loop is rendered by a second
client at <http://127.0.0.1:1234>. Same keys, same state, no second editor. The
web client is a demo, not a promised frontend — but it is the proof that “state
is a value” is not a slogan here.

The rest follows from the same decision:

- **Crash-safe by construction.** Every edit appends a small journal record
  before the next key is read. `kill -9` loses at most the command in flight;
  restart replays the journal. There is a pty test that edits, kills the JVM,
  restarts, and requires the text back.
- **One painter.** The region, search matches, LSP diagnostics and diff faces
  are all durable `[from to)` props with a face and a priority. They follow
  edits until the text they cover disappears. Fontification is derived per
  line, cached, and pruned to the viewport, so an edit only drops the lines it
  can affect.
- **No plugin API to learn first.** If the editor can do it, it is a function
  you can call: `(swap! ed/editor ed/find-file "src/nesmacs/ed.clj")`.

## What is in the box

- **Editing** — persistent char-array text with a line index, Unicode and
  wide-character aware
  cursor math, undo/redo with adjacent inserts coalesced, bracketed paste as
  one edit, mouse clicks that move point, OSC 52 copy so kills reach the
  system clipboard (even over SSH).
- **Finding** — incremental search, query-replace, occur, imenu, goto-line,
  window tree with splits and resizing, project search via ripgrep streamed
  into a buffer.
- **Files and projects** — find-file, dired, buffer list, recentf, bookmarks,
  registers, rectangles, project find-file.
- **Programs** — `M-!` and `M-|` are jobs: output streams into a buffer while
  typing stays responsive, `C-c C-k` kills, `g` reruns.
- **HTTP** — `M-x http` opens `*HTTP*`; the request sits above the first
  blank line, `C-c C-c` sends it and writes the response below. The second
  non-editor app built from the same primitives: a buffer, a mode keymap and
  a command.
- **Git** — `C-x g` status with stage/unstage, diff, log and commit. Diff and
  log buffers are ordinary buffers with faces and navigation. `C-x v b` blame,
  `C-x v s`/`C-x v u` stage or unstage one hunk through `git apply --cached`,
  `C-x v p` lists pull/merge requests via `gh` or `glab`.
- **Languages** — one registry (`nesmacs.lang`) maps extension to mode, comment
  syntax, formatter, language server, Tree-sitter grammar and query set.
  Clojure/EDN, Python, JavaScript, TypeScript and TSX highlight through
  vendored nvim-treesitter queries, with the line tokenizer as fallback.
  `C-c TAB` folds, `C-c f` formats.
- **LSP** — diagnostics as props, `M-.` definition, `C-c h` hover, `C-c i`
  symbol completion (honouring `textEdit` and snippets), `C-c r` references,
  `C-c R` rename, `C-c a` code actions, `C-c g` signature help, `C-c m`
  document symbols, `C-c f`/`C-c F` format buffer/region. One server per
  project root and language; servers start on demand.
- **Debugging** — `C-c d d` starts the language's DAP adapter (debugpy,
  vscode-js-debug; Clojure keeps nREPL). `C-c d b` toggles a gutter
  breakpoint, `c`/`n`/`i`/`o` resume and step, `s`/`v` show frames and locals,
  `e` evaluates in the stopped frame, `q` disconnects. Adapters are data in
  `nesmacs.lang`; `make dap` runs the live debugpy and vscode-js-debug tests.
- **Tests** — `M-x run-tests` runs the project's command as a job and parses
  JUnit XML or TAP into `*Test Results*`; RET jumps to a failure. Runners with
  their own format still yield jumpable entries from `file:line` lines.
- **Durability** — session restore for buffers, windows, points and props;
  per-command journal; auto-save with a recovery prompt; safe quit dialog.
- **Rendering** — one named face map with two palettes (`M-x load-theme`:
  `default`, `light`) plus `api/set-face!` for one-off tweaks; durable props,
  incremental font-lock, a one-column gutter for breakpoints and the stopped
  line, large-file mode above 1 MB or 100k lines (the region still paints
  there).
- **AI assistance** — `M-x agent` opens the configured CLI (`$NESMACS_AGENT`, or
  `claude`) in a normal terminal buffer. The editor has no API client.

Packages are Clojure libraries on `deps.edn`; there is no package manager yet.

## Configure it in Clojure

`~/.nesmacs/init.clj` runs at startup, after the previous session is restored.
`M-x reload-config` re-evaluates it without restarting.

```clojure
(ns my.init
  (:require [nesmacs.api :as api]))

(defn greet
  "Say hello in the echo area."
  [s] (assoc s :msg "hello from init.clj"))

(api/bind! "C-c g" #'greet)               ; named commands also appear in M-x

(api/register-mode! :notes-mode :major
  {"C-c C-c" (fn [s] (assoc s :msg "noted"))})

(api/add-hook! :clojure-mode-hook
  (fn [s] s))
```

A failing `init.clj` reports in the echo area and the editor keeps running.
Text properties are public too: `(api/add-prop! 0 12 :match {:owner :demo})`,
`(api/set-prop! :hover 20 30 :builtin)`, `(api/remove-prop! :demo)` — the same
durable ranges search and diagnostics use.

## How it is built

```
read-key -> handle-key(state, key) -> next state -> render(state) -> terminal
     ^                                                                |
     '------------------------- command loop ------------------------'
```

One atom, pure commands, side effects only at the boundary. The state can be
tested, replayed from a journal, and rendered by another process.

| Module | Role |
| --- | --- |
| `nesmacs.ed` | editor core: commands, built-in keymap, modes, main loop |
| `nesmacs.buf`, `nesmacs.gap`, `nesmacs.text` | buffer type, text storage + line index, text primitives |
| `nesmacs.window`, `nesmacs.view` | window tree and layout; the only screen painter |
| `nesmacs.props`, `nesmacs.face` | durable text properties, tokenizer |
| `nesmacs.api`, `nesmacs.keymap`, `nesmacs.mode` | extension surface, key parsing, modes and hooks |
| `nesmacs.proc`, `nesmacs.job`, `nesmacs.lsp` | processes, streaming job buffers, language server |
| `nesmacs.lang`, `nesmacs.treesit`, `nesmacs.fold`, `nesmacs.format`, `nesmacs.snippet` | language registry, Tree-sitter backend, folding, CLI formatters, LSP snippets |
| `nesmacs.terminal`, `nesmacs.terminal.tmux` | terminal buffers behind a backend protocol; the tmux control-mode backend |
| `nesmacs.git*`, `nesmacs.githunk`, `nesmacs.gitblame`, `nesmacs.forge`, `nesmacs.testres`, `nesmacs.project`, `nesmacs.dired`, `nesmacs.buflist`, `nesmacs.nav`, `nesmacs.complete` | git apps (status, log, hunk staging, blame, forge), test reports, project search, file and buffer navigation, completion |
| `nesmacs.session`, `nesmacs.persist`, `nesmacs.term` | session and journal, persistence, raw terminal I/O |

Design rules live in [PRD.md](PRD.md): commands stay pure, one atom, one
rendering pipeline, one keymap mechanism, no second process model. A shortcut
with a known limit carries a `shortcut:` comment naming when to upgrade it.

## Development

```sh
make dev     # run from source
make test    # 669 tests / 6,549 assertions
make check   # lint + tests + pty smoke + kill -9 recovery + require
make bench   # timing gates (kept out of test so a loaded box can't fail CI)
make lsp     # live tests against clojure-lsp, pyright and typescript-language-server
make term    # terminal session test against a real tmux
make dap     # live debugger tests against debugpy and vscode-js-debug
make grammars # resolve every Tree-sitter grammar into an empty Maven repo
```

The pty scripts start the editor in a real terminal, drive it with keys and
check the rendered frame, including blame/hunk/test-report/forge list flows.
Terminal buffers are exercised end-to-end against real tmux by `make term`,
which is excluded from `make test` like the live `lsp`, `dap` and `grammars`
targets. A live target skips a server or adapter it cannot find; point
`NESMACS_CLOJURE_LSP`, `NESMACS_PYRIGHT`, `NESMACS_TSSERVER`, `NESMACS_DEBUGPY` or
`JS_DEBUG_DAP` at an install outside `PATH`.
The benchmark asserts under 50 ms per keystroke on a
100k-line file and under 2 s to open 10 MB; measured roughly 20 ms and 30 ms on
a 16-core Linux machine. Everything is testable from `clojure.test`, including
generative tests for buffers, props and text.

## Status, and when not to use it

Pre-1.0. Solid editing and text properties are done; async jobs, project
navigation, git, LSP, completion and tmux-backed terminals have working slices.
The language platform (M8: Tree-sitter highlighting and folding, with queries
vendored from nvim-treesitter under Apache-2.0, plus LSP breadth for Clojure/EDN,
Python, TypeScript and JavaScript) and the IDE workflow (M9: blame, hunk
staging, forge, test results, snippets) are implemented. M10 adds the DAP
debugger, themes, the agent CLI and the HTTP app.
[ROADMAP.md](ROADMAP.md) tracks the rest and defines 1.0: a full week as the
primary editor without falling back to another tool.

If you need org-mode, Magit, a GUI, or your existing `~/.emacs.d` today, use
Emacs — it is 40 years of work and this is not. nesmacs is for the opposite itch:
an editor small enough to read, where extending it means writing a function and
the running editor is debuggable from the REPL. No Elisp, no GUI frames, no
user native extension modules; a vetted library such as Tree-sitter sits behind
an optional backend.

Contributions are welcome. Read the milestone in [ROADMAP.md](ROADMAP.md), keep
commands pure and reach new behavior through `nesmacs.api`; data structures need
tests, terminal behavior needs a pty script.

## License

MIT — see [LICENSE](LICENSE).