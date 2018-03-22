repl:
	lein run

test:
	lein run 4 5 \* p 355 113. \/ . 0 q

jar:
	lein uberjar

deploy:
	lein deploy clojars

.PHONY: test
