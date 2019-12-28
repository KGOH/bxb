repl:
	rm -rf .cpcache/ && DEBUG=true clj -A:dev:test:nrepl

build:
	clj -A:build
	mv target/app-1.0.0-SNAPSHOT-standalone.jar app.jar

run-jar:
	java -jar app.jar -m app.core

test:
	DEBUG=false clj -A:test:runner


