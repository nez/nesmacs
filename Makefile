JAR := target/xmas.jar

.PHONY: dev run jar repl test pty lint check clean setup

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

# native: requires GraalVM native-image (not in CI). Dropped per M7 scope.
# To re-enable: install GraalVM, uncomment below, and verify.
# NATIVE := xmas-native
# native: jar
# 	native-image -jar $(JAR) --no-fallback --initialize-at-build-time -o $(NATIVE)

test:
	clojure -M:test

pty:
	python3 test/pty/smoke.py

lint:
	clj-kondo --lint src

check: lint test
	clojure -M -e '(require (quote xmas.ed)) (println "ok")'

clean:
	rm -rf target .cpcache .nrepl-port $(NATIVE)
