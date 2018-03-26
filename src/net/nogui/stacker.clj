(ns
  ^{:doc "Stacker - stack based language with repl interface -- similar to Forth/Factor"
    :author "Jörg Ramb, 2017-2018"}
  net.nogui.stacker
  (:gen-class :main true)
  (:require [clojure.string :as str]
            [instaparse.core :as instaparse]
            [clojure.pprint :as pp])
  (:import
    ;;(java.util LinkedList)
    (jline.console ConsoleReader)
    (jline.console.completer AnsiStringsCompleter)
    (jline.console.completer CandidateListCompletionHandler)
    (jline.console.completer Completer)
    (jline.console.completer FileNameCompleter)
    (jline.console.completer StringsCompleter)
    ))

(def color
  {:green "\u001B[32m"
   :blue "\u001B[34m"
   :cyan "\u001B[36m"
   :red "\u001B[31m"
   :purple "\u001B[35m"
   :white "\u001B[37m"
   :reset "\u001B[0m"})

(defn in-color [col s]
  (str (col color) s (:reset color)))

(def prompt (in-color :green "> "))

(defn spop [s]
  (if (empty? s)
    [s nil]
    [(pop s) (peek s)]))

(defn top-if [stack condition else]
  (let [t (peek stack)]
    (if (condition t) t else)))

(defn exit [s env]
  (let [t (peek s)]
    (System/exit (if (number? t) t 0))))

;; (drop n s) returns a lazy list which makes problems later? FIXME (but this works)
(defn n-drop [n s]
  (if (or (<= n 0) (empty? s))
    s
    (recur (dec n) (pop s))))

(defmacro safe-fn [f & args]
  `(try
     (~f ~@args)
     (catch Exception e# (println (in-color :red (str "Error: " e#))))))

(defn func2
  "Takes a binary function and returns a stacker-function which expects two
  items on the stack, performs the function on it and pushes the result back on
  the stack. In other words: it makes a ( a b -- (f b a) )"
  [f]
  (fn [s env]
    (let [[s a] (spop s)
          [s b] (spop s)]
      [(conj s (safe-fn f b a)) env])))

(defn func1
  "Takes a unary function and returns a stacker-function which expects one
  item on the stack, performs the function on it and pushes the result back on
  the stack. In other words: it makes a ( a -- (f a) )"
  ([f]
   (fn [s env]
     (let [[s a] (spop s)]
       [(conj s (safe-fn f a)) env])))
  ([env f]
   (fn [s env]
     (let [[s a] (spop s)]
       [(conj s (safe-fn f env a)) env]))))

(defn func1-env
  "Takes a binary function and returns a stacker-function which expects one
  item on the stack, performs the function on it (also adds the env) and pushes the result back on
  the stack. In other words: it makes a ( a -- (f a env) )"
  [f]
  (fn [s env]
    (let [[s a] (spop s)]
      [(conj s (safe-fn f a env)) env])))

(defn func3
  "Takes a ternary function and returns a stacker-function which expects two
  items on the stack, performs the function on it and pushes the result back on
  the stack. In other words: it makes a ( a b c -- (f c b a) )"
  [f]
  (fn [s env]
    (let [[s a] (spop s)
          [s b] (spop s)
          [s c] (spop s)]
      [(conj s (safe-fn f c b a)) env])))

;; All stacker functions take a stack and an env and return a stack and an env
(defn safe-drop [s env]
  [(if (empty? s) s (pop s)) env])

(defn safe-print [stack-item]
  ;; FIXME: make this better
  (str stack-item))

(defn print-stack [stack]
  (println "S:" (str/join ", " (map safe-print (reverse stack)))))

(defn print-env [env]
  (println "E:" (sort (map str (keys env)))))

(def parser
  (instaparse/parser ;; EBNF
   (slurp (clojure.java.io/resource "net/nogui/stacker.bnf"))))

(declare quotation-to-fn) ; mutual with compile-tokens

(defn compile-tokens
  "compiles tokens and returns a function which takes a stack and an env and returns a
  possible update [stack env] pair."
  [env tokens]
  ;; (println "compiling" tokens (empty? tokens))
  (if (empty? tokens)
    (fn [s e] [s e]) ;; identity
    (let [[kind value]  (first tokens)
          remaining     (rest tokens)]
      ;; (println "kind " kind " value " value " type" (type value))
      (condp = kind
        :word (let [w (get env value)]
                (if-let [f (quotation-to-fn env w)]
                  (if (:mod-env w)
                    ;; function can modify the environment
                    (fn [s e]
                      (let [[s e] (f s e)]
                        ((compile-tokens e remaining) s e)))
                    ;; function should not modify the environment
                    (fn [s e]
                      (let [[s _] (f s e)]
                        ((compile-tokens e remaining) s e))))
                  ;; else unknown word
                  (do
                    (if (:-strict env)
                      (do
                        (println "*** Word not defined:" value)
                        (fn [s e] [s e]))
                      ;; else push value on stack
                      (fn [s e] ((compile-tokens env remaining) (conj s value) e))))))
        :quotation (let [q (rest value) ; value = [:S [:word ..] []...]
                         v {:fn (compile-tokens env q)}]
                     (fn [s e] ((compile-tokens env remaining) (conj s v) e)))
        :sexp (let [v (eval (read-string value))
                    v (if (fn? v) {:fn v} v)]
                (fn [s e] ((compile-tokens env remaining) (conj s v) e)))
        :reader (let [v (read-string value)]
                  (fn [s e] ((compile-tokens env remaining) (conj s v) e)))
        :keyword (let [v (keyword value)]
                   (fn [s e] ((compile-tokens env remaining) (conj s v) e)))
        :str (let [v (read-string value)]
               (fn [s e] ((compile-tokens env remaining) (conj s v) e)))))))

(defn apply-tokens
  [stack env tokens]
  (let [f (compile-tokens env tokens)]
    (f stack env)))

(defn quotation-to-fn [env q]
  (or (:fn q)
      (if-let [tokens (:quotation q)]
        (compile-tokens env tokens))))

(defn string-to-tokens
  "Parses the string s into tokens."
  [str]
  (let [p (parser str)]
    ;;(pp/pprint p)
    (when-let [tokens (rest p)]
      ;;(pp/pprint tokens)
      tokens)))

(defn do-test [id test result check env]
  (if test
    (let [[s2 env] (apply-tokens () env (string-to-tokens test))
          [s3 env] (apply-tokens () env (string-to-tokens result))
          ok? (= s2 s3)]
      (println (str (if ok?
                      (str (:green color) "PASS")
                      (str (:red color) "FAIL"))
                    (:reset color) ":") id "\t" (in-color :cyan test)
               "-->" (in-color (if ok? :green :red) s2)
               (str (when (not ok?) (str "expected " (in-color :green s3)))))
      ok?)
    (do
      #_(println "SKIP: No test defined for" id)
      true)))

(def default-env
  (atom {
         "." {:signature "(a -- )"
              :doc "Pops the top of the stack and displays it."
              :fn (fn [s env]
                    (println (peek s))
                    (safe-drop s env))}
         "+" {:signature "(n1 n2 -- n3)"
              :fn (func2 +)}
         "++" {:signature "(id -- n)"
               :doc "increments the environment id and returns the new value on the stack."
               :test [["5 :x set :x ++" "6"]]
               :mod-env true
               :fn (fn [s env]
                     (let [[s id] (spop s)
                           val (inc (or (get env id) 0))]
                       [(conj s val) (assoc env id val)]))}
         "-" {:signature "(n1 n2 -- n3)"
              :fn (func2 -)}
         "--" {:signature "(id -- n)"
               :doc "decrements the environment id and returns the new value on the stack."
               :test [["5 :x set :x --" "4"]]
               :mod-env true
               :fn (fn [s env]
                     (let [[s id] (spop s)
                           val (dec (or (get env id) 0))]
                       [(conj s val) (assoc env id val)]))}
         "*" {:signature "(n1 n2 -- n3)"
              :fn (func2 *)}
         "/" {:signature "(n1 n2 -- n3)"
              :fn (func2 /)}
         ">" {:signature "(n1 n2 -- bool)"
              :fn (func2 >)}
         ">=" {:signature "(n1 n2 -- bool)"
               :test [["3 -3 >= [1] [0] if" "1"]]
               :fn (func2 >=)}
         "<=" {:signature "(n1 n2 -- bool)"
               :test [["-3 -3 <= [1] [0] if" "1"]]
               :fn (func2 <=)}
         "<" {:signature "(n1 n2 -- bool)"
              :test [["-3 -1 < [1] [0] if" "1"]]
              :fn (func2 <)}
         "=" {:signature "(a b -- bool)"
              :fn (func2 =)}
         "and" {:signature "(bool-1 bool-2 -- bool-3)"
                :fn (func2 (fn [a b] (and a b)))}
         "or" {:signature "(bool-1 bool-2 -- bool-3)"
               :fn (func2 (fn [a b] (or a b)))}
         "not" {:signature "(a b -- bool)"
                :doc "negates the top of the stack."
                :test [["3 4 > not" "2 2 ="]]
                :fn (func1 not)}
         "clear" {:signature "(? -- )"
                  :doc "Clears the stack completely."
                  :fn (fn [s env] [() env])}
         "inc" {:signature "(n1 -- n2)"
                :test [["4 inc" "5"]]
                :fn (func1 inc)}
         "dec" {:signature "(n1 -- n2)"
                :test [[ "4 dec" "3"]]
                :fn (func1 dec)}
         "compile" {:signature "(q -- q)"
                    :doc "Compiles the quotation on the top of the stack. Multiple applications are possible, but meaningless"
                    :test [["\"22 7 /\" parse compile apply" "22 7 /"]
                           ["\"dup *\" parse compile \"sqr\" set 4 sqr" "16"]
                           ["\"42\" parse compile compile compile apply" "42"]]
                    :fn (func1-env
                         (fn [q env]
                           (let [src (:quotation q)]
                             (if (and src (not (:fn q)))
                               {:fn (compile-tokens env src) :quotation src}
                               q))))}
         "parse" {:signature "(str -- q)"
                  :doc "parses the string str and leaves the result as an (uncompiled) quotation on the stack."
                  :test [["\"22 7 /\" parse apply" "22 7 /"]
                         ["\"dup *\" parse \"sqr\" set 4 sqr" "16"]]
                  :fn (fn [s env]
                        (let [[s string] (spop s)]
                          [(conj s {:quotation (string-to-tokens string)}) env]))}
         "join" {:signature "(seq str-delim -- str-joined)"
                 :doc "joins the elements of seq with str in between."
                 :test [["1 5 range \", \" join" "\"1, 2, 3, 4, 5\""]
                        ["1 8 range nil join"  " \"12345678\" "]]
                 :fn (func2 (fn [seq s]
                              (str/join s seq)))}
         "n-param" {:signature "(n -- )"
                    :doc "takes the n next items from the stack and converts them into environment variables, :a, :b, ..."
                    :test [["10 20 30 3 n-param :c get :a get +" "40"]]
                    :mod-env true
                    :fn (fn [s env]
                          (let [[s n] (spop s)]
                            (if (<= 1 n 26)
                              (loop [s s i n env env]
                                (if (> i 0)
                                  (let [[s v] (spop s)]
                                    (recur s (dec i) (assoc env (keyword (str (char (+ i -1 (int \a))))) v)))
                                  [s env]))
                              (do
                                (println "*** number of params should be 1 <= n <= 26")
                                [s env]))))}
         "param" {:signature "([a b...] -- )"
                  :doc "takes the linear quotation (must only contain ids) from the stack and saves the corresponding stack items."
                  :test [["10 20 30 [:a _ :c] param :c get :a get +" "40"]
                         ["990 10 [:x] param :x get +" "1000"]]
                  :mod-env true
                  :fn (fn [s env]
                        (let [[s q] (spop s)
                              param-fn (quotation-to-fn env q)
                              [param-names _] (param-fn () env)]
                          (loop [s s
                                 env env
                                 seq param-names]
                            (if (empty? seq)
                              [s env]
                              (let [[s v] (spop s)
                                    id (peek seq)]
                                (recur s (assoc env id v) #_(if (= id "_")
                                                              env ;; skip this
                                                              (assoc env id v))
                                       (pop seq)))))))}
         "grab" {:signature "(a b c n -- [a b c])"
                 :doc "grabs the top n elements and makes them a sequence which is pushed back on the stack."
                 :fn (fn [s env]
                       (let [[s n] (spop s)]
                         [(conj (n-drop n s) (reverse (take n s))) env]))}
         "count-old" {:signature "(seq -- a)"
                      :doc "counts the number of elements in the sequence."
                      :test [["2 19 inc inc range count-old" "20"]]
                      :quotation (string-to-tokens "[1] map [+] reduce")
                      }
         "count" {:signature "([a b...] -- [a b c] n)"
                  :doc "counts the sequence on top and puts the count (only) on the stack."
                  :test [["2 19 inc inc range count" "20"]]
                  :fn (fn [s env]
                        (let [[s sq] (spop s)]
                          [(conj s (count (seq sq))) env]))}
         "test" {:signature "(id -- bool)"
                 :doc "Performs a self-test (if defined) on the word. Leaves the result of the tests on the stack."
                 :fn (func1-env (fn [id env]
                                  (let [tests (:test (get env id))]
                                    (reduce (fn [a b] (and a b)) true
                                            (for [[test result check] tests]
                                              (do-test id test result check env))))))}
         "if" {:signature "(bool q-true q-false -- ?)"
               :test [["4 5 > [ :yes ] [ :no ] if" ":no"]]
               :doc "if bool is true, apply q-true, otherwise apply q-false. "
               :fn (fn [s env]
                     (let [[s else] (spop s)
                           [s when] (spop s)
                           [s chck] (spop s)
                           f (quotation-to-fn env (if chck when else))]
                       (f s env)))}
         "doc" {:signature "(id -- )"
                :fn (fn [s env]
                      (let [[s id] (spop s)
                            e (get env id)]
                        (if e
                          (do
                            (println "### " id "--" (or (:signature e) "(? -- ?)"))
                            (when (:doc e)
                              (println (:doc e)))
                            (dorun
                             (for [test (:test e)]
                               (println "Example: " (first test) "-->" (second test)))))
                          (println "Unknown:" id)))
                      [s env])}
         "p" {:signature "(a -- a)"
              :fn (fn [s env]
                    (println (peek s))
                    [s env])}
         "get" {:signature "(a -- b)"
                :fn (fn [s env]
                      (let [[s id] (spop s)]
                        [(conj s (get env id)) env]))}
         "set" {:signature "(a-value id -- )"
                :doc "sets the entry named id to a-value in the env. a-value can be a quotation."
                :mod-env true
                :test [["113. :r set 355 :r get /" "3.1415929203539825"]
                       ["-1 :a set [5 :a set [99 :a set] apply :a get dup *] apply :a get" "25 -1"]
                       ["[[:ok] \"get-ok\" set get-ok] apply" ":ok"]]
                :fn (fn [s env]
                      (let [[s id] (spop s)
                            [s v] (spop s)]
                        [s (assoc env id v)]))}
         "apply" {:signature "(q -- ?)"
                  :test [["-1 4 5 [+] apply" "-1 9"]]
                  :fn (fn [s env]
                        (let [[s q] (spop s)
                              f (quotation-to-fn env q)]
                          (f s env)))}
         "until" {:signature "(q-body q-test -- ?)"
                  :doc "applies q-body on the stack repeatedly until q-test returns true. "
                  :test [["4 [inc] [dup 10 >=] until" "10"]]
                  :fn (fn [s env]
                        (let [[s q-test] (spop s)
                              f-test (quotation-to-fn env q-test)
                              [s q-body] (spop s)
                              f-body (quotation-to-fn env q-body)]
                          ;; the env is reused during while, but reset afterwards
                          (loop [s s env-while env]
                            (let [[s env-while] (f-test s env-while)
                                  [s result] (spop s)]
                              (if-not result
                                (let [[s env-while] (f-body s env-while)]
                                  (recur s env-while))
                                [s env])))))}
         "while" {:signature "(q-body q-test -- ?)"
                  :doc "as long as the q-test returns true, apply q-body."
                  :test [["4 [inc] [dup 10 >=] until" "10"]]
                  :fn (fn [s env]
                        (let [[s q-test] (spop s)
                              f-test (quotation-to-fn env q-test)
                              [s q-body] (spop s)
                              f-body (quotation-to-fn env q-body)]
                          ;; the env is reused during while, but reset afterwards
                          (loop [s s env-while env]
                            (let [[s env-while] (f-test s env-while)
                                  [s result] (spop s)]
                              (if result
                                (let [[s env-while] (f-body s env-while)]
                                  (recur s env-while))
                                [s env])))))}
         "do"    {:signature "(seq -- seq)"
                  :doc "realizes a potential lazy sequence"
                  :fn (func1 doall)}
         "load"  {:signature "(f -- ?)"
                  :doc "loads the filename f, parse and applies the contents."
                  :mod-env true
                  :fn (fn [s env]
                        (let [[s file] (spop s)
                              tokens (string-to-tokens (slurp file))
                              f (compile-tokens env tokens)]
                          ;; load will/can modify the env
                          (f s env)))}
         "range" {:signature "(n1 n2 -- seq)"
                  :doc "returns a lazy sequence from n1..n2 (note: including both n1 and n2). If n2<n1 the sequence is reversed."
                  :fn (func2 (fn [a b]
                               (if (<= a b)
                                 (range a (inc b))
                                 (range a (dec b) -1))))}
         "drop"  {:signature "(a -- )"
                  :fn safe-drop}
         "skip"  {:signature "(seq skip-num -- seq)"
                  :doc "skips the first skip-num elements from the sequence."
                  :test [["1 20 range 18 skip [+] reduce" "39"]]
                  :fn (func2 (fn [sequence skip-num]
                               (drop skip-num (seq sequence))))}
         "take"  {:signature "(seq take-num -- seq)"
                  :doc "takes the first take-num elements from the sequence."
                  :fn (func2 (fn [sequence take-num]
                               (take take-num (seq sequence))))}
         "reverse"  {:signature "(seq -- seq)"
                     :doc "reverses the sequence."
                     :fn (func1 (fn [sequence]
                                  (reverse (seq sequence))))}
         "reduce" {:signature "(seq1 q -- a)"
                   :doc "reduces the sequence using the quotation q and leaves the result on the stack."
                   :test [["1 10 range [*] reduce" "3628800"]]
                   :fn (fn [s env]
                         (let [[s reduce-q] (spop s)
                               f (quotation-to-fn env reduce-q)
                               [s sequence] (spop s)
                               sequence (seq sequence)]
                           [(conj s
                                  (reduce (fn [a b]
                                            ;; will throw away interim envs
                                            (first (first (f (conj s a b) env)))) sequence)) env]
                           ))}
         "map"   {:signature "(seq1 q -- seq2)"
                  :fn (fn [s env]
                        (let [[s map-q] (spop s)
                              f (quotation-to-fn env map-q)
                              [s sequence] (spop s)]
                          [(conj s
                                 ;; will throw away interim envs
                                 (map #(first (first (f (conj s %) env))) (seq sequence))) env]
                          ))}
         "peek"  {:signature "(a -- a)"
                  :fn (fn [s env]
                        (let [v (peek s)]
                          (println v "=" (type v)))
                        [s env])}
         "dup"   {:signature "(a -- a a)"
                  :test [["7 dup" "7 7"]]
                  :doc "Duplicates the top of the stack."
                  :fn (fn [s env] [(conj s (peek s)) env])}
         "stack" {:signature "( -- )"
                  :fn (fn [s env]
                        (print-stack s)
                        [s env])}
         "env"   {:signature "( -- )"
                  :fn (fn [s env]
                        [(conj s (sort (map str (keys env)))) env])}
         "exit"  {:signature "( -- )"
                  :fn exit}
         "q"     {:signature "( -- )"
                  :fn exit}
         "swap" {:signature "(a b -- b a)"
                 :fn (fn [s env]
                       (let [[a b & r] s]
                         (if (>= (count s) 2)
                           [(conj (conj r a) b) env]
                           [s env])))}
         }))



(defn make-line-reader []
  (let [cr (ConsoleReader.)]
    (.setPrompt cr prompt)
    (.addCompleter cr (StringsCompleter. (keys @default-env)))
    (.addCompleter cr (FileNameCompleter.))
    cr))

(defn repl [start-stack env start-words]
  (let [lr (make-line-reader)
        ;; env @default-env
        start-tokens (string-to-tokens (str/join " " start-words))
        initial (apply-tokens start-stack env start-tokens)]
    (loop [[stack env] initial]
      ;;(print-stack stack)
      ;; (print-env env)
      (let [r (.readLine lr prompt)
            tokens (string-to-tokens r)]
        (if (or (not tokens) (= r "bye"))
          [stack env] ;; return the stack and current env
          (recur (apply-tokens stack env tokens)))))))


(defn -main [& args]
  #_(when (not (nil? args))
    (println "args: " args))
  (let [[s env] (repl () @default-env args)
        top (first s)]
    (when (number? top)
      (System/exit top))))

