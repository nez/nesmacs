JAR := target/xmas.jar

.PHONY: dev run jar repl test bench pty lint check clean setup

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
	clojure -M:test -e :benchmark

# Timing assertions only; excluded from `test` so a loaded box can't fail CI.
bench:
	clojure -M:test -i :benchmark

pty:
	python3 test/pty/smoke.py

lint:
	clj-kondo --lint src

check: lint test pty
	clojure -M -e '(require (quote xmas.ed)) (println "ok")'

clean:
	rm -rf target .cpcache .nrepl-port
