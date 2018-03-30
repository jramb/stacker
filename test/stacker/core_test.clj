(ns stacker.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [net.nogui.stacker :refer :all]))

(defmacro hide-output [& body]
  `(binding [*out* (io/writer "/dev/null")]
     ~@body))

(deftest basics
  (testing "basics"
    (let [engine (make-engine)]
      (is (= nil (peek-stack engine)))
      ;;
      (feed-engine engine "355 113. /")
      (is (= 3.1415929203539825 (peek-stack engine)))
      (feed-engine engine "drop")
      (is (= nil (peek-stack engine)))
      ;;
      (feed-engine-seq engine ["123" "321" "+"])
      (is (= 444 (peek-stack engine)))
      (feed-engine engine "drop")
      (is (= nil (peek-stack engine)))
      ;;
      (feed-engine-multi engine "1 1337" "range" "[+]" "reduce")
      (is (= 894453 (peek-stack engine)))
      (feed-engine engine "drop")
      (is (= nil (peek-stack engine)))
      ;;
      (comment
        (hide-output
         (feed-engine engine "env [test] map do")
         (await engine))
        (let [v (peek-stack engine)]
          (is (reduce #(and %1 %2) v) "testing all env"))
        (feed-engine engine "drop"))
      ;;
      (feed-engine engine "\"Hej\"")
      (is (= "Hej" (peek-stack engine)))
      (feed-engine engine "drop")
      (is (= () (get-stack engine)) "stack is now empty")
      )))

(deftest built-in-tests
  (testing "Basically this: env [test] map"
    (let [engine (make-engine)]
      (feed-engine engine "env")
      (doseq [cmd (peek-stack engine)]
        (hide-output
         (feed-engine-multi engine (str \" cmd \") " test"))
        (is (= true (peek-stack engine)) (str "testing " cmd))
        (push-stack engine "drop")))))

(deftest define-words
  (testing "Define words"
    (let [engine (make-engine)]
      (engine-set-word engine "push-dummy"
                       {:fn (fn [s e] [(conj s "dummy") e])})
      (feed-engine engine "push-dummy")
      (is (= "dummy" (peek-stack engine))))))

(deftest performance
  (testing "Performance"
    (let [engine (make-engine)]
      (hide-output
       (feed-engine engine "20 :depth set mandelbrot.stkr load"))
      (await engine))
    (is (= 1 1))))

(deftest push-pop
  (testing "push-pop"
    (let [test-value 1337
          engine (make-engine)]
      (push-stack engine test-value)
      ;; (await engine)
      (let [on-stack (peek-stack engine)
            [s _] @engine]
        (is (= s (list test-value)))
        (is (= test-value on-stack))))))
