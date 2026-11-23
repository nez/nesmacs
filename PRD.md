# xmas — Clojure-native editor with the Emacs paradigm

## Goal

Keep the Emacs model — buffers, windows, keymaps, modes, hooks, commands as
`state -> state` transitions — on a small Clojure core. Extend it in Clojure,
not in a second language.

Every feature must collapse onto a small set of primitives: buffers, pure
commands, gap text, one editor atom. Resist parallel implementations of the
same idea; share the primitive instead of duplicating it.

## Non-goals

- Running Emacs Lisp or `~/.emacs.d`. The elisp runtime was removed; it is
  recoverable at tag `elisp-runtime-v0` but is not maintained.
- GNU `lisp/*.el`, `package.el`, quelpa, or Emacs third-party packages.
- Bytecode, native modules, GUI frames.

Packages are Clojure libraries on `deps.edn`.

## Current state

The editor implements the M0-M7 scope in [ROADMAP.md](ROADMAP.md): Emacs-style
editing, durable text properties, async jobs, project navigation, completion,
git, LSP and crash-safe sessions. [README.md](README.md) has the current feature
list, test counts and benchmarks. v1.0 is still pending a full week of
primary-editor use.

Configuration is `~/.xmas/init.clj` via `xmas.api` (`bind!`, `add-hook!`,
`register-mode!`, `add-prop!`, `set-prop!`, `remove-prop!`); `M-:` evaluates
Clojure in the same context.

## Roadmap

Milestones, scope and the remaining decision live in [ROADMAP.md](ROADMAP.md).
M0-M7 are implemented; v1.0 is pending a full week of primary-editor use.

## Principles

- Commands stay pure; side effects only at the boundary (`save-buffer`, I/O).
- One atom, `swap!` at the loop boundary; state is a value you can test,
  replay, and render from another process (the web client proves it).
- A shortcut with a known limit gets a `shortcut:` comment.