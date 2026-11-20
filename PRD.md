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
major/minor modes with keymaps and hooks, overlays, isearch, query-replace,
keyboard macros, registers, bookmarks, recentf, dired, buflist, shell
commands, auto-save, terminal and web frontends, nREPL on 7888.

Configuration is `~/.xmas/init.clj` via `xmas.api` (`bind!`, `add-hook!`,
`register-mode!`); `M-:` evaluates Clojure in the same context.

## Next

1. **Text properties** — replace the per-mode tokenizer in `face.clj` with
   faces stored on buffer ranges, so `font-lock`-style highlighting and
   overlays share one mechanism.
2. **Completion** — in-buffer completion menu and async candidates; the
   minibuffer already supports live filtering and Tab completion.
3. **Project search** — ripgrep-backed `M-x rg`, results in a `listbuf`.
4. **Git** — status/diff buffers (the first non-editor app on the platform).
5. **LSP** — `future`-driven client; would remove Emacs's single-thread
   limitation as a reason to keep a separate editor.

## Principles

- Commands stay pure; side effects only at the boundary (`save-buffer`, I/O).
- One atom, `swap!` at the loop boundary; state is a value you can test,
  replay, and render from another process (the web client proves it).
- A shortcut with a known limit gets a `shortcut:` comment.