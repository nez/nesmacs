JAR := target/xmas.jar

.PHONY: dev run jar repl test bench lsp term dap grammars pty crash lint check clean setup

setup:
	./install.sh

dev:
	clojure -M:run

repl:
	clojure -M -e '(require (quote xmas.ed)) (clojure.main/repl)'

# Source startup loads ~40 namespaces (~4 s); the AOT jar starts in ~1.5 s.
# Rebuild only when a source or dependency file changed, so `make run` is the
# fast path once the jar exists.
run: $(JAR)
	java -jar $(JAR)

$(JAR): $(shell find src resources -type f) deps.edn
	clojure -X:uberjar

jar:
	clojure -X:uberjar

test:
	clojure -M:test -e :benchmark -e :integration

# End-to-end against every configured language server (clojure-lsp, pyright,
# typescript-language-server); a missing one is a skip. XMAS_CLOJURE_LSP,
# XMAS_PYRIGHT and XMAS_TSSERVER point at installs outside PATH.
lsp:
	clojure -M:test -n xmas.lsp-live-test

# End-to-end against debugpy and vscode-js-debug. Needs `pip install debugpy`
# (XMAS_DEBUGPY points at that Python) and JS_DEBUG_DAP=.../dapDebugServer.js.
dap:
	clojure -M:test -n xmas.dap-live-test

# Resolve every pinned Tree-sitter grammar from an empty local Maven repo, so a
# warm ~/.m2 cannot hide a missing artifact or native library.
grammars:
	clojure -Sdeps '{:mvn/local-repo ".cpcache/m2-check"}' -M:test -n xmas.grammar-live-test

# End-to-end terminal session against the real tmux binary; needs tmux on PATH.
term:
	clojure -M:test -n xmas.terminal-live-test

# Timing assertions only; excluded from `test` so a loaded box can't fail CI.
bench:
	clojure -M:test -i :benchmark

pty:
	python3 test/pty/smoke.py
	python3 test/pty/lists.py

# kill -9 mid-edit, restart, require the journal to restore the edit.
crash:
	python3 test/pty/crash_recovery.py

lint:
	clj-kondo --lint src

check: lint test pty crash
	clojure -M -e '(require (quote xmas.ed)) (println "ok")'

clean:
	rm -rf target .cpcache .nrepl-port
