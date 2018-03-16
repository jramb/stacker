repl:
	lein run

test:
	lein run 4 5 \* p 355 113. \/ p q

deploy:
	lein deploy clojars

.PHONY: test
