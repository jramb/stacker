version=0.2.1-SNAPSHOT

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

performance:
	 time -v -o mandelbrot.performance lein run mandelbrot.stkr load q >mandelbrot.performance.txt

deploy:
	lein deploy clojars

.PHONY: test
