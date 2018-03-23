repl:
	lein run

test:
	lein run 4 5 \* p 355 113. \/ . 0 q
	lein run test1.stkr load

jar:
	lein uberjar
	cd target && ln -s stacker-0.1.1-standalone.jar stacker.jar

deploy:
	lein deploy clojars

.PHONY: test
