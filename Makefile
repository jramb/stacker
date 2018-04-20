version=0.3.1-SNAPSHOT

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

# alternative way of running things
clj:
	clj -m net.nogui.stacker

js:
	clj -m cljs.main -c net.nogui.stacker -r

# clj -m cljs.main --target node --output-to stacker.js --optimizations simple -c net.nogui.stacker

.PHONY: test
