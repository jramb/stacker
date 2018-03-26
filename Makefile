version=0.2.0

repl:
	lein run

test:
	lein run 4 5 \* p 355 113. \/ . 0 q
	lein run test1.stkr load q
	lein run 'env [test] map do q'

jar:
	lein uberjar
	cd target && ln -s stacker-$(version)-standalone.jar stacker.jar
	cp target/stacker-$(version)-standalone.jar share/stacker.jar

deploy:
	lein deploy clojars

.PHONY: test
