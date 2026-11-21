JAR := target/xmas.jar

.PHONY: dev run jar repl test bench lsp pty crash lint check clean setup

setup:
	./install.sh

dev:
	clojure -M:run

repl:
	clojure -M -e '(require (quote xmas.ed)) (clojure.main/repl)'

run: jar
	java -jar $(JAR)

jar:
	clojure -X:uberjar

test:
	clojure -M:test -e :benchmark -e :integration

# End-to-end against the real clojure-lsp binary; needs it on PATH.
lsp:
	clojure -M:test -i :integration

# Timing assertions only; excluded from `test` so a loaded box can't fail CI.
bench:
	clojure -M:test -i :benchmark

pty:
	python3 test/pty/smoke.py

# kill -9 mid-edit, restart, require the journal to restore the edit.
crash:
	python3 test/pty/crash_recovery.py

lint:
	clj-kondo --lint src

check: lint test pty crash
	clojure -M -e '(require (quote xmas.ed)) (println "ok")'

clean:
	rm -rf target .cpcache .nrepl-port
