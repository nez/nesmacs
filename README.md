# xmas

**A text editor with the Emacs paradigm, written in Clojure.**

Buffers, windows, keymaps, modes, hooks and commands as pure `state -> state`
functions — no Emacs Lisp runtime, no `~/.emacs.d`. Everything you extend is
Clojure running in the same process as the editor.

```text
(ns xmas.hello
  (:require [xmas.api :as api]))

(defn greet
  "Say hello in the echo area."
  [s]
  (assoc s :msg "hello from init.clj"))

(api/bind! "C-c g" #'greet)

 -- src/xmas/hello.clj   L7 C22  (clojure-mode)
```

## Why another editor

- **Your fingers already know it.** `C-f`, `C-x C-s`, `C-s`, `M-%`, `M-x`,
  `C-x 2`, `C-x C-c`: prefix keys, numeric arguments, mark, region, kill ring.
- **One language.** Configuration, modes, commands and fontification are
  Clojure. `~/.xmas/init.clj` runs against the live editor, `M-:` evaluates in
  it, and nREPL is already listening on `:7888`.
- **State is a value.** One atom, commands that take a state and return the
  next one. The web client on `:1234` drives that same command loop from a
  browser — that is the point, not a gimmick.

Status: pre-1.0 and used daily for the milestone loop in
[ROADMAP.md](ROADMAP.md). It is not a drop-in Emacs replacement, and it does
not try to be: no Elisp, no GUI frames, no packages.

## Quick start

Requires Java 11+, the [Clojure CLI](https://clojure.org/guides/install_clojure)
and a POSIX terminal (`stty`). Developed on Linux.

```sh
git clone https://github.com/nez/xmas.git
cd xmas
make dev
```

`make dev` runs from source. `make run` builds `target/xmas.jar` first;
`./xmas` runs that jar directly. On Debian/Ubuntu, `./install.sh` installs the
toolchain and runs the full check.

First launch creates `~/.xmas/` for configuration, session and journal. Quit
with `C-x C-c`; it asks to save each modified buffer instead of dropping work.

Optional but recommended: `ripgrep` for project search, `clojure-lsp` for LSP,
`git` for the git buffers, `clj-kondo` for lint, `python3` for the pty tests.

## What's inside

| Area | What works |
| --- | --- |
| Editing | Gap buffer with a line index, Unicode and wide-character aware cursor math, undo/redo with adjacent inserts coalesced into one step, bracketed paste as one edit, OSC 52 copy on kill |
| Navigation | Incremental search, query-replace, occur, imenu, goto-line, window tree with splits, resizing and per-window scroll |
| Files and projects | find-file, dired, buffer list, recentf, bookmarks, registers, rectangles, project find-file, ripgrep search streamed into a job buffer |
| Programs | `M-!` and `M-|` as async jobs; a job owns a buffer, streams output, and can be killed (`C-c C-k`) or rerun (`g`) without freezing the editor |
| Git | Status with stage/unstage, diff, log, commit. Diff and log buffers are ordinary buffers with faces and navigation |
| LSP | Diagnostics as durable text properties, go-to-definition (`M-.`), hover (`C-c h`), symbol completion (`C-c i`); any server via `:lsp-command`, `clojure-lsp` by default |
| Completion | Minibuffer prefix completion with history, `M-x` commands and symbol-at-point, all from the same primitive |
| Durability | Session restore (buffers, windows, points, props), a per-command journal that survives `kill -9`, auto-save with a recovery prompt on startup, safe quit dialog |
| Rendering | One named face map, durable props for anything that outlives a frame, per-line font-lock cached and pruned to the viewport, large-file mode above 1 MB or 100k lines |
| Extensibility | `xmas.api`: `bind!`, `add-hook!`, `register-mode!`, `add-prop!`, `set-prop!`, `remove-prop!`, `configure-lsp!`. Reload `init.clj` with `M-x reload-config` |

Highlighting ships for Clojure/EDN and Emacs Lisp. There is no plugin
ecosystem yet: packages are Clojure libraries on `deps.edn`.

## Keys you will use

| Key | Command |
| --- | --- |
| `C-x C-f` | find file |
| `C-x C-s` | save buffer |
| `C-x C-c` | quit, asking per modified buffer (`y/n/!`) |
| `C-x b` / `C-x C-b` | switch buffer / buffer list |
| `C-x d` | dired |
| `C-x g` | git status |
| `C-x p f` / `C-x p g` | project file / ripgrep search |
| `C-x 2` `C-x 3` `C-x o` `C-x 0` `C-x 1` | split, other window, delete window(s) |
| `C-s` / `C-r` | incremental search forward/backward |
| `M-%` | query-replace |
| `M-s o` | occur |
| `M-i` / `M-g` | imenu / goto line |
| `M-.` / `C-c h` / `C-c i` | LSP definition / hover / complete symbol |
| `C-x (` `C-x )` `C-x e` | record / stop / replay a keyboard macro |
| `C-x r …` | registers, rectangles, bookmarks |
| `M-!` / `M-\|` | async shell command / filter region through a command |
| `M-x` / `M-:` | run a command / evaluate Clojure |

Full keymap: [`src/xmas/ed.clj`](src/xmas/ed.clj).

## Configure it in Clojure

`~/.xmas/init.clj` runs at startup, after the previous session is restored:

```clojure
(ns my.init
  (:require [xmas.api :as api]))

(defn greet
  "Say hello in the echo area."
  [s] (assoc s :msg "hello from init.clj"))

(api/bind! "C-c g" #'greet)               ; named commands also appear in M-x

(api/register-mode! :notes-mode :major
  {"C-c C-c" (fn [s] (assoc s :msg "compiled"))})

(api/add-hook! :clojure-mode-hook
  (fn [s] s))
```

A failing `init.clj` reports in the echo area and the editor keeps running.
`M-x reload-config` re-evaluates it without restarting.

Text properties are part of the public surface — the same durable ranges that
search matches, diagnostics and diffs use:

```clojure
;; From M-: or nREPL, where the live editor is in scope
(api/add-prop! 0 12 :match {:owner :demo})   ; paint, accumulating
(api/set-prop! :hover 20 30 :builtin)        ; replace one owner's range
(api/remove-prop! :demo)                     ; clear an owner
```

Props follow edits until the text they cover disappears.

## Live hacking

The running editor is an atom you can inspect and rewrite. nREPL listens on
`7888`; connect from CIDER, Calva, or the command line:

```clojure
(require '[xmas.ed :as ed] '[xmas.cmd :as cmd])

(keys @ed/editor)                      ; the whole state
(str (:text (cmd/cur @ed/editor)))     ; current buffer text
(swap! ed/editor cmd/edit 0 0 "hello") ; inject an edit
(swap! ed/editor ed/find-file "/tmp/x.clj")
(swap! ed/editor ed/switch-buffer "*scratch*")

(defn my-cmd "It works." [s] (ed/msg s "it works"))
(xmas.api/bind! "M-z" #'my-cmd)        ; also visible to M-x
```

Changes appear on the next redisplay (any keypress). The dev watcher reloads
changed namespaces from `src/`. The web demo at
<http://127.0.0.1:1234> is the same loop rendered in a browser.

## How it works

```
read-key -> handle-key(state, key) -> next state -> render(state) -> terminal
     ^                                                                |
     '------------------------- command loop ------------------------'
```

One atom holds the editor. Commands are pure functions of that state; side
effects happen only at the boundary (`save-buffer`, processes, terminal I/O).
That is what makes the state testable, replayable from a journal, and
renderable by a second frontend.

- **Buffers** are immutable maps over a gap buffer with a line index. Every
  mutation bottoms out in `buf/edit`, which adjusts point, undo, props,
  font-lock and the crash journal in one place.
- **Windows** are a tree; layout, splits and scroll positions are derived from
  it, never stored twice.
- **Painting** has one path: `xmas.props` holds durable ranges, `xmas.face`
  tokenizes a line, `xmas.view` applies props over spans, and the region is
  just a high-priority prop created at render time. Font-lock is derived per
  line and pruned to the viewport, so edits drop only what they can affect.
- **Extensions** reach the editor through `xmas.api` and the dynamic
  `*state*`, so configuration runs inside the command loop's transaction.

| Module | Role |
| --- | --- |
| `xmas.ed` | editor core: commands, built-in keymap, modes glue, main loop |
| `xmas.buf`, `xmas.gap`, `xmas.text` | buffer type, gap buffer + line index, text primitives |
| `xmas.window` | window tree, layout, splits |
| `xmas.props`, `xmas.face`, `xmas.view` | text properties, tokenizer, screen painting |
| `xmas.term` | raw terminal input/output, clipboard, resize signals |
| `xmas.mode`, `xmas.api`, `xmas.keymap` | modes, hooks and the extension surface |
| `xmas.proc`, `xmas.job` | processes and streaming job buffers |
| `xmas.project`, `xmas.nav`, `xmas.complete`, `xmas.dired`, `xmas.buflist` | project search, occur/imenu, completion, file and buffer navigation |
| `xmas.git*`, `xmas.lsp` | git status/diff/log/commit, language server client |
| `xmas.session`, `xmas.persist` | session snapshots, crash journal, auto-save |
| `xmas.web`, `xmas.dev`, `xmas.log` | browser demo, source watcher, debug log |

## Development

```sh
make dev     # run from source
make jar     # build target/xmas.jar
make test    # 512 tests / 6,095 assertions
make check   # lint + test + pty smoke + kill -9 recovery + require
make bench   # timing gates (excluded from test so a loaded box can't fail CI)
make lsp     # integration tests against a real clojure-lsp
```

The pty scripts start the editor in a real terminal, drive it with keystrokes,
and check the rendered frame; the crash test edits, kills the JVM, restarts and
requires the journal to restore the edit. `make bench` asserts under 50 ms per
keystroke on a 100k-line file and under 2 s to open 10 MB (measured ~20 ms and
~28 ms on a 16-core Linux machine).

Design rules the code follows, in [PRD.md](PRD.md): commands stay pure, one
atom, one rendering pipeline, one keymap mechanism, no second process model. A
shortcut with a known limit carries a `shortcut:` comment naming when to
upgrade it.

## Roadmap

Solid editing (M0) and text properties (M1) are in. Later milestones have
working slices — async jobs, project navigation, git, LSP, completion — and
what remains is tracked in [ROADMAP.md](ROADMAP.md), including the definition
of 1.0: a full week of primary-editor work without falling back to another
tool.

## License

MIT — see [LICENSE](LICENSE).