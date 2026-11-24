# xmas — Clojure-native IDE with the Emacs paradigm

## Goal

Keep the Emacs model — buffers, windows, keymaps, modes, hooks, commands as
`state -> state` transitions — on a small Clojure core. Extend it in Clojure,
not in a second language.

Grow that core into a full-featured IDE by reusing protocols and external
tools, not by reimplementing them: LSP for language intelligence, Tree-sitter
for syntax, the project's own formatters, linters, test runners, git, ripgrep
and tmux. Per-language behavior is data (a mode registry), not code.

Every feature must collapse onto a small set of primitives: buffers, pure
commands, gap text, one editor atom. Resist parallel implementations of the
same idea; share the primitive instead of duplicating it.

## Non-goals

- Running Emacs Lisp or `~/.emacs.d`. The elisp runtime was removed; the last
  version is reachable from `main` at commit `276dd47` but is not maintained.
- GNU `lisp/*.el`, `package.el`, quelpa, or Emacs third-party packages.
- Bytecode, user native extension modules, GUI frames. A vetted native library
  behind an optional backend (Tree-sitter) is not a user extension module.

Packages are Clojure libraries on `deps.edn`.

## Current state

The editor implements the M0-M9 scope in [ROADMAP.md](ROADMAP.md): Emacs-style
editing, durable text properties, async jobs, project navigation, completion,
git, LSP, crash-safe sessions, tmux-backed terminals, the language platform
(Tree-sitter highlighting and folds, LSP breadth, formatting) and the IDE
workflow (blame, hunk staging, forge, test reports, snippets). M10 (debugging
and platform extras) is post-v1.0. [README.md](README.md) has the current
feature list, test counts and benchmarks. v1.0 is still pending a full week of
primary-editor use.

Terminals are core, not an experiment. `xmas.terminal` keeps the editor side
pure — screen text, props and an input queue — behind a `Backend` protocol;
`xmas.terminal.tmux` runs each shell in a private tmux server. tmux owns the
PTY and VT emulation and the editor paints `capture-pane` snapshots, so there
is no second terminal emulator in this codebase. Sessions outlive the editor
and reattach on the next start.

Configuration is `~/.xmas/init.clj` via `xmas.api` (`bind!`, `add-hook!`,
`register-mode!`, `add-prop!`, `set-prop!`, `remove-prop!`); `M-:` evaluates
Clojure in the same context.

## Roadmap

Milestones, scope and the settled decisions live in [ROADMAP.md](ROADMAP.md).
M0-M9 are implemented; M10 is post-v1.0. v1.0 is pending a full week of
primary-editor use.

## Principles

- Commands stay pure; side effects only at the boundary (`save-buffer`, I/O).
- One atom, `swap!` at the loop boundary; state is a value you can test,
  replay, and render from another process (the web client proves it).
- Language features come from protocols (LSP, DAP), vendored query sets
  (nvim-treesitter) and existing binaries (git, rg, tmux, formatters), not
  from new implementations.
- Per-language behavior is data: one mode registry, not one module per
  language. A new panel is a list buffer, not a new widget system.
- A shortcut with a known limit gets a `shortcut:` comment.