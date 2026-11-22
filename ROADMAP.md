# xmas roadmap

Scope for the Clojure-native editor. Product definition, milestones, and the
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
7. survive `kill -9` with no lost buffer content.

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

## Starting point (already built)

Gap buffer with line index, immutable buffer + undo/redo, window tree, major and
minor modes with hooks, minibuffer with prefix completion and history,
isearch, query-replace, keyboard macros, registers, bookmarks, recentf,
dired, buffer list, auto-save, blocking shell commands, terminal and web
frontends, nREPL, file watcher, `~/.xmas/init.clj` with `xmas.api`.

Roughly 3.6k lines of `src`, 3k of `test`, 417 tests.

Known gaps that the milestones below close: no clipboard or bracketed paste,
undo is per keystroke, no crash recovery, no async process model, no project
model, no completion beyond the minibuffer, no git, no LSP.

---

## M0 — Solid editing

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

**Not here:** clipboard *reads* (terminals cannot), selection/mouse, themes.

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

**Not here:** tree-sitter, semantic highlighting, folds.

Size M. Risk medium: property adjustment is the classic off-by-one graveyard;
property tests are the exit criterion. Covered by targeted boundary tests and
`test.check` properties over random edits.

## M2 — Processes and jobs

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

## M3 — Projects and navigation

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

## M4 — Completion

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

## M5 — Git (the first app)

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

## M6 — LSP

**Goal:** diagnostics, completion, hover and definition for real projects.

**Scope**

- JSON-RPC 2.0 over stdio on top of `xmas.proc`; document sync; server
  lifecycle (start on project open, restart on crash, shutdown on exit).
- Diagnostics as M1 props; hover in the echo area; `M-.` definition;
  completion as an M4 source.
- Start with `clojure-lsp`; keep the client language-agnostic.
- `make lsp` runs the end-to-end check against the real `clojure-lsp` binary
  (diagnostics, hover, completion, definition, crash restart); it is excluded
  from `make test` because it needs the server and its indexing time.
- Evaluate `lsp4clj` before hand-rolling the protocol layer.

**Done when:** clojure-lsp shows squiggles and completions, `M-.` jumps,
killing the server restarts it, and a slow server never blocks typing.

**Not here:** rename, code actions, workspace edits, semantic tokens, inlay
hints, debug adapter.

Bail-out: if the protocol layer is not working in two weeks, ship regex
completion + imenu `M-.` and push LSP past v1.0. This milestone is allowed to
slip; it is the largest risk in the roadmap.

Size L. Risk high.

## M7 — Durability, performance, packaging

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
- `make native` verified for the current feature set, or dropped from the docs.
- Benchmarks as tests where stable; pty smoke and a `kill -9` recovery test in
  `make check`.

**Done when:** `kill -9` + restart restores the session, a 100k-line file is
usable, and no failure mode leaves a broken terminal.

**Not here:** undo persistence, crash reporting/telemetry, auto-update.

Size M. Risk medium.

## M8 — Platform extras (optional, after v1.0)

Only after v1.0, and only with a concrete use:

- One non-editor app beyond git (HTTP client or feed reader — `http-kit` is
  already a dependency), to test the platform claim.
- Themes, mouse support, tabs, multiple cursors, tree-sitter, folding.
- Native packaging for other platforms.

Nothing in this milestone is promised.

---

## Order and cut line

Dependencies: M1 ← nothing; M2 ← nothing; M3 ← M2; M4 ← M3; M5 ← M2 (+M1 for
diff faces); M6 ← M1, M2, M4; M7 ← all; M8 ← v1.0.

If time runs out, cut from the bottom: **M8 → M6 → M5 → M4**. The irreducible
core is **M0–M3**: correct editing, one painting mechanism, non-blocking
processes, and project navigation. That alone is a usable editor.

## Permanent non-goals

- Emacs Lisp, GNU `lisp/*.el`, `~/.emacs.d`, `package.el`, magit/org parity.
- GUI frames, a terminal emulator inside the editor, email/IRC/feed clients
  (except as the single M8 experiment).
- Collaborative editing, multi-user sessions, Windows terminal support.
- Plugin sandboxing: configuration is the user's own code, trusted like any
  `init` file.

## Open decisions

1. **Props vs overlays** — resolved in M1: durable props for annotations,
   derived per-line tokenization for syntax. No overlay module.
2. **Completion UI** — minibuffer first (recommended); in-buffer popup only if
   the minibuffer proves insufficient.
3. **Job writes** — every background mutation is a pure `swap!`; jobs never
   call commands that assume they are on the command-loop thread.
4. **LSP implementation** — `lsp4clj` versus a ~300-line hand-rolled client.
   Spike both for one day at the start of M6.
5. **Java/Clojure floor** — pin the minimum version in `deps.edn` and in
   `install.sh` before M7 packaging.

## Measurement

- Tests: `make check` stays green at every commit; new data structures get
  property tests.
- Interactive: scripted pty smoke for startup, config errors, and clean exit.
- Performance: the M7 targets, measured on the user's machine, not synthetic
  hardware.
- Product: the seven-day v1.0 loop, with zero data-loss incidents.