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

Text editor with Emacs keybindings: gap buffer with line index, window tree,
major/minor modes with keymaps and hooks, edit-tracking text properties,
isearch, query-replace, keyboard macros, registers, bookmarks, recentf, dired,
buflist, shell commands, auto-save, terminal and web frontends, nREPL on 7888.

Configuration is `~/.xmas/init.clj` via `xmas.api` (`bind!`, `add-hook!`,
`register-mode!`, `add-prop!`, `set-prop!`, `remove-prop!`); `M-:` evaluates
Clojure in the same context.

## Roadmap

Milestones, scope, cut line and open decisions live in [ROADMAP.md](ROADMAP.md).
The short version: solid editing → text properties → async processes →
projects → completion → git → LSP → durability.

## Principles

- Commands stay pure; side effects only at the boundary (`save-buffer`, I/O).
- One atom, `swap!` at the loop boundary; state is a value you can test,
  replay, and render from another process (the web client proves it).
- A shortcut with a known limit gets a `shortcut:` comment.