# xmas roadmap

Scope for the Clojure-native IDE. Product definition, milestones, and the
things this project deliberately will not build. See [PRD.md](PRD.md) for the
principles behind it and [README.md](README.md) for the current code.

## Definition of v1.0

xmas is 1.0 when it can be the primary editor for a full week without falling
back to another tool for this loop:

1. open and edit files in a project (including a 100k-line file),
2. search the project and jump to a result,
3. complete symbols while typing,
4. stage, diff and commit in git,
5. run the project's tests and read the output,
6. reload configuration without restarting,
7. survive `kill -9` with no lost buffer content,
8. edit Clojure/EDN, Python, TypeScript and JavaScript with syntax
   highlighting and LSP breadth (references, rename, code actions, format).

M8 closes item 8; M9 closes the daily workflow around it. Debugging and the
remaining platform extras (M10) are explicitly post-v1.0.

One non-editor application must exist on the same primitives (git status is
the first; see M5), because "the editor is the first app" is the platform
claim this project is testing.

## Scope rules

- Everything must reach the editor through the existing surface: a buffer, a
  mode with a keymap, `state -> state` commands, one editor atom.
- A feature that needs a second rendering pipeline, a second keymap mechanism
  or a second process model is out of scope until the existing one is fixed.
- Data structures get tests. Terminal behaviour gets a scripted pty smoke test,
  not a manual checklist.
- The web client is a demo of "state is a value", not a supported frontend. It
  never gates a milestone.

## Status

Milestones M0-M7 are implemented to the scope described below. M8 (language
platform) and M9 (IDE workflow) are accepted and must ship before v1.0; M10
(debugging and platform extras) is post-v1.0. v1.0 is not declared yet: it
requires M8/M9 plus a full week as the primary editor (see
[Definition of v1.0](#definition-of-v10)). Current features, test counts and
benchmarks live in [README.md](README.md); this file keeps the milestone record
and the settled decisions.

The plan started from an editor with the Emacs editing primitives already in
place. The gaps it set out to close — clipboard and bracketed paste, undo
grouping, crash recovery, async jobs, project navigation, completion, git and
LSP — are now closed. M8-M9 extend the same primitives into a language platform
and the daily IDE loop instead of adding a second renderer, parser or process
model. tmux-backed terminals ship as core infrastructure: the editor paints
snapshots from a backend protocol, so a polling or pty4j backend could replace
tmux without touching the editor side.

---

## M0 — Solid editing (shipped)

**Goal:** never lose or corrupt the user's text.

**Scope**

- Bracketed paste (`?2004`) in raw mode: one insert, one undo entry, no
  per-character undo flood, bounded time for a 1 MB paste.
- Kill ring ↔ system clipboard via OSC 52 (works over SSH). Copy on kill;
  paste stays the terminal's job.
- Undo grouping: one undo boundary per command; adjacent inserts coalesce.
  Redo unchanged.
- Safe quit: `C-x C-c` with modified buffers asks to save each (`y/n/!`),
  not just warn.
- Crash recovery: on startup, find `#file#` auto-saves and offer to restore.
- Scripted pty test for start → key → exit and for config-load errors.

**Done when:** pasting 1 MB is one undo, `C-k` lands in the system clipboard,
killing the JVM and restarting restores the buffer, and a bad `init.clj` is
reported without breaking startup.

**Not here:** clipboard *reads* (terminals cannot), text selection, themes.

Size S–M. Risk low.

## M1 — Text properties (shipped)

**Goal:** one mechanism for "paint this range": region, search matches,
diagnostics and diffs.

**Built**

- `xmas.props` holds durable `{:from :to :face :priority :owner}` annotations
  on the buffer and adjusts them on every edit; search, query-replace, LSP
  diagnostics and diff faces all write props. Extensions use
  `xmas.api/add-prop!`, `set-prop!` and `remove-prop!`.
- The region is a render-time prop (`:owner :region`, high priority), so it
  goes through the same painter without being stored or adjusted. It still
  paints in large-file mode, where buffer props are skipped.
- Fontification is incremental too: `:font-lock` caches `[spans state]` per
  line, edits drop it from the edited line down, and the renderer reuses the
  cached spans instead of re-tokenizing visible lines every frame. Spans for
  lines outside the viewport are pruned on scroll, keeping only the cheap lexer
  states; large-file mode skips the cache entirely. This is the same span
  painter, not a second overlay system.
- `xmas.view/faces` is the single named face map; terminal and web share it.

**Done when:** deleting text inside a highlighted range leaves correct props,
search matches and the region use the same mechanism, and the overlay
namespace is gone. All three hold, and `init.clj` can add and remove props
through `xmas.api`.

**Not here:** tree-sitter, semantic highlighting, folds. Tree-sitter and folds
are revisited in M8 as an optional backend; semantic highlighting stays out.

Size M. Risk medium: property adjustment is the classic off-by-one graveyard;
property tests are the exit criterion. Covered by targeted boundary tests and
`test.check` properties over random edits.

## M2 — Processes and jobs (shipped)

**Goal:** stop freezing the editor on anything that takes time.

**Scope**

- `xmas.proc`: `ProcessBuilder` with argv vectors (never a shell string),
  cwd/env, stdout/stderr filters, exit sentinel, kill, reaping on exit.
- Background writes go through pure `swap!` functions; a job owns its buffer.
- `xmas.job`: a process-backed buffer (listbuf pattern) with streaming append,
  `C-c C-k` kill, `g` rerun, and a mode keymap.
- Convert long shell commands to jobs; keep blocking only for region filters
  whose output replaces the region.
- No zombie processes when the editor exits.

**Done when:** a `find /`-sized job streams into a buffer while typing stays
responsive, and quitting kills its children.

**Not here:** a general async/await framework, thread pools, remote processes.

Size M. Risk medium: concurrency is where the "one atom, pure commands"
invariant pays off or breaks.

## M3 — Projects and navigation (shipped)

**Goal:** work inside a repository without leaving the editor.

**Scope**

- Project root detection (`.git`, `deps.edn`, `project.clj`, `.project`).
- `project-find-file`: fuzzy filename completion, honours `.gitignore`
  (minimal matcher; no full gitignore engine until it breaks).
- Project search via `rg` as an M2 job; results buffer jumps to file:line.
- `occur` in the current buffer; `imenu`-style symbol list per mode using the
  existing tokenizer, no parser.
- `M-x reload-config` re-runs `init.clj` in place.

**Done when:** from any file in a repo: find a file by partial name, search a
regex across the project, jump to a symbol in the current file, reload config.

**Not here:** ctags/TAGS, multi-root workspaces, remote (TRAMP-like) files.

Size S–M. Risk low.

## M4 — Completion (shipped)

**Goal:** completion that does not block and can be fed by more than one
source.

**Scope**

- Async source protocol: `(fn [prefix ctx] -> future of candidates)`, with a
  registry per mode and a global default.
- Sources: buffers, files, project files, current-buffer symbols.
- Minibuffer shows a vertical, annotated candidate list with `C-n/C-p`; no
  in-buffer popup in this milestone.
- Cache project file lists per project root, invalidated on demand.

**Done when:** typing in the minibuffer filters asynchronously, and a Clojure
symbol from project sources completes at point without an LSP server.

**Not here:** fuzzy ranking beyond prefix/substring, snippets, company-style
popups.

Size M. Risk low–medium.

## M5 — Git, the first app (shipped)

**Goal:** the daily commit loop, and proof that an "app" is cheap here.

**Scope**

- `git status --porcelain=v2 -z`, `git diff`, stage/unstage, commit, log.
- Status and diff buffers on `listbuf` + M2 jobs; diff faces via M1 props.
- Argv vectors only; no shell interpolation of paths or messages.
- Document the app recipe (buffer + mode + keymap + job) in the README.

**Done when:** a full commit (inspect, stage, message, commit) happens without
leaving xmas, and refresh does not block input.

**Not here:** magit parity, rebase UI, hunk-level staging (until asked),
blame, forge/PR integration.

Size M. Risk low.

## M6 — LSP (shipped, scoped)

**Goal:** diagnostics, completion, hover and definition for real projects.

**Scope**

- JSON-RPC 2.0 over stdio on top of `xmas.proc`; document sync; server
  lifecycle (start on project open, restart on crash, shutdown on exit).
- Diagnostics as M1 props; hover in the echo area; `M-.` definition;
  completion as an M4 source.
- Start with `clojure-lsp`; the protocol layer is generic, but document sync is
  limited to Clojure/EDN files.
- `make lsp` runs the end-to-end check against the real `clojure-lsp` binary
  (diagnostics, hover, completion, definition, crash restart); it is excluded
  from `make test` because it needs the server and its indexing time.
- The protocol layer is hand-rolled on `xmas.proc`; there is no LSP library
  dependency.

**Done when:** clojure-lsp shows squiggles and completions, `M-.` jumps,
killing the server restarts it, and a slow server never blocks typing.

**Not here:** rename, code actions, workspace edits, semantic tokens, inlay
hints, debug adapter.

Bail-out: if the protocol layer is not working in two weeks, ship regex
completion + imenu `M-.` and push LSP past v1.0. This milestone is allowed to
slip; it is the largest risk in the roadmap.

Size L. Risk high.

## M7 — Durability, performance, packaging (shipped)

**Goal:** use it all day, every day, without thinking about it.

**Scope**

- Session restore: buffers, windows, points, recent files (undo history is
  explicitly not persisted).
- Edit journal: each command appends its text edits to `~/.xmas/journal.edn`,
  so `kill -9` loses at most the command in flight. On restart the journal
  replays over the last snapshot (trimmed to keep it small) and is then folded
  into a new snapshot. A save also records a marker, so edits already in the
  file are not replayed twice.
- Large-file mode: above a threshold, skip fontification and property work,
  render lazily. Targets: open 10 MB < 2 s; keystroke on 100k lines < 50 ms.
- Terminal robustness: resize during render, suspend/resume (`C-z`), a command
  that throws never leaves the terminal in raw mode or the screen corrupt.
- Benchmarks as tests where stable; pty smoke and a `kill -9` recovery test in
  `make check`.

**Done when:** `kill -9` + restart restores the session, a 100k-line file is
usable, and no failure mode leaves a broken terminal.

**Not here:** undo persistence, crash reporting/telemetry, auto-update.

Size M. Risk medium.

## M8 — Language platform (accepted, before v1.0)

**Goal:** edit the four supported languages as first-class languages without
writing parsers, language servers or per-language code. Everything
language-specific is data; the editor supplies one parser backend and one
protocol client.

**Settled decisions**

- Java floor is 17, enforced by `install.sh`; `deps.edn` pins Clojure 1.12.4.
- Tree-sitter is an optional runtime backend: `io.github.bonede/tree-sitter`
  plus per-grammar Maven artifacts. That binding supports JDK 8+, so Java 17
  leaves room to move to the official binding later.
- Queries come from nvim-treesitter (Apache-2.0) or the grammar repositories
  (MIT), with attribution. GNU Emacs `treesit` queries are GPLv3 and are not
  copied while xmas stays MIT.
- Languages: Clojure/EDN, Python, TypeScript, JavaScript. Emacs Lisp keeps its
  existing mode; unknown extensions open as plain text.
- Debugging is post-v1.0 (M10).

**Scope**

1. Mode registry as data: extension -> mode, comment syntax, formatter, LSP
   command, grammar, query set, indentation. Adding a language is data.
2. A `:parser` per buffer behind a small interface: incremental edit,
   byte->char offset conversion, parsing outside the render loop, skipped in
   large-file mode. A missing grammar or native library falls back to the
   tokenizer or plain text.
3. Highlighting for the four languages by mapping query captures to
   `xmas.view/faces`.
4. LSP breadth on the existing client: references, rename, code actions,
   signature help, document symbols, formatting and range formatting, and
   workspace edits for multi-file changes.
5. Format-on-save: LSP formatting first, per-mode CLI fallback (`cljfmt`,
   `ruff format`, `prettier`).
6. Folding and imenu from the Tree-sitter tree once highlighting is stable.
7. Tests: parser and offset unit tests, query mapping, a pty highlighting
   smoke, and `make lsp` coverage for each supported server. A missing grammar
   or server is a covered path, never a crash.

**Done when:** `.clj`, `.py`, `.ts` and `.js` open with correct highlighting;
references, rename, code actions and format work through the LSP client;
removing a grammar or server degrades to plain text without breaking startup;
large files stay inside the M7 keystroke budget.

**Not here:** debugging, semantic tokens, inlay hints, tree-sitter for every
language, writing grammars or queries from scratch, GPL query sets.

Size L. Risk high: native code can end the JVM, and offset conversion is a
classic off-by-one source. The journal bounds text loss; a grammar crash is an
accepted, documented risk until a sidecar proves necessary.

## M9 — IDE workflow (accepted, before v1.0)

**Goal:** close the daily IDE loop around the language platform without
building a second UI or a second process model.

**Scope**

1. Git history and staging: blame, hunk-level staging through
   `git apply --cached`, and forge actions from `gh`/`glab` CLI output. No API
   clients.
2. Test results: run the project's test command as a job and parse JUnit XML
   or TAP into a list buffer that jumps to failures. One parser, not one per
   framework.
3. Snippets: advertise LSP `snippetSupport` and expand server-provided
   snippets with a minimal expander; no snippet library of our own.
4. Reuse the existing list buffer, minibuffer and props for every panel.

**Done when:** blame, stage-by-hunk, a PR list and a failing test run are
usable without leaving xmas, and none of them blocks typing.

**Not here:** debug adapter, magit parity, a test framework, a second widget
system.

Size M. Risk low-medium.

## M10 — Debugging and platform extras (post-v1.0, optional)

Only after v1.0, and only with a concrete use:

- A DAP client on a JSON-RPC transport generalized from `xmas.lsp`; adapters
  are reused (`debugpy`, `delve`, `lldb-dap`, `java-debug`). Breakpoints,
  stack, variables, evaluate and output reuse list buffers and the minibuffer.
- One non-editor app beyond git (HTTP client or feed reader — `http-kit` is
  already a dependency), to test the platform claim.
- Themes, tabs, multiple cursors, semantic tokens, inlay hints.
- An external agent CLI in a terminal buffer for AI assistance; no API client
  in the editor.

Nothing in this milestone is promised.

---

## Order and cut line (historical)

This is the original sequencing; M0-M7 are implemented. M8 and M9 are the
accepted additions that gate v1.0; M10 is post-v1.0 and not promised.
Dependencies: M1 ← nothing; M2 ← nothing; M3 ← M2; M4 ← M3; M5 ← M2 (+M1 for
diff faces); M6 ← M1, M2, M4; M7 ← all; M8 ← M1, M4, M6; M9 ← M2, M5;
M10 ← v1.0.

If time runs out before v1.0, cut in this order: **M9's forge/tests/snippets,
then M8's folding/imenu**. Keep M8's mode registry, highlighting and LSP
breadth: they are the language bar. The irreducible core is still **M0-M3**:
correct editing, one painting mechanism, non-blocking processes, and project
navigation.

## Permanent non-goals

- Emacs Lisp, GNU `lisp/*.el`, `~/.emacs.d`, `package.el`, magit/org parity.
- GUI frames, email/IRC/feed clients (except as the single M10 experiment).
- A VT emulator of our own: terminals run under tmux and the editor paints
  captures. A different backend must still fit the `xmas.terminal/Backend`
  protocol.
- A second extension runtime or arbitrary native modules. A vetted native
  library behind an optional backend (Tree-sitter) is allowed.
- Collaborative editing, multi-user sessions, Windows terminal support.
- Plugin sandboxing: configuration is the user's own code, trusted like any
  `init` file.

## Decisions

The props/overlay, completion-UI, job-write and LSP-implementation questions
were settled in their milestones. The language milestone settled the rest:

- **Java 17** is the floor (`install.sh`); **Clojure 1.12.4** is pinned in
  `deps.edn`.
- **Tree-sitter** is an optional backend, not a foundation. Binding:
  `io.github.bonede/tree-sitter`; queries: nvim-treesitter (Apache-2.0) or
  grammar repositories (MIT); no GNU Emacs GPLv3 queries while xmas is MIT.
- **Supported languages**: Clojure/EDN, Python, TypeScript, JavaScript;
  unknown extensions fall back to plain text.
- **Debugging** (DAP) is post-v1.0.

No decisions block v1.0. Grammar pins and DAP adapter coverage are maintenance,
not architecture.

## Measurement

- Tests: `make check` stays green at every commit; new data structures get
  property tests.
- Interactive: scripted pty smoke for startup, config errors, clean exit and
  language highlighting.
- Languages: `make lsp` covers each supported server; a missing grammar or
  server is a tested fallback path.
- Performance: the M7 targets, measured on the user's machine, not synthetic
  hardware.
- Product: the seven-day v1.0 loop, with zero data-loss incidents.