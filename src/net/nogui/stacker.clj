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

(def stats
  (atom nil))

(defn stats-inc [key]
  (when @stats
    (swap! stats #(assoc % key (inc (get % key 0))))))

;; activate this to disable all stats (gives a tiny bit more performance)
;; (defmacro stats-inc [key])

(def color
  {:green  "\u001B[32m"
   :blue   "\u001B[34m"
   :cyan   "\u001B[36m"
   :red    "\u001B[31m"
   :purple "\u001B[35m"
   :white  "\u001B[37m"
   :reset  "\u001B[0m"})

(defn in-color [col s]
  (str (col color) s (:reset color)))

(def prompt (in-color :green "> "))

(defn top-if [stack condition else]
  (let [t (first stack)]
    (if (condition t) t else)))

(defn exit [s env]
  (let [t (first s)]
    (System/exit (if (number? t) t 0))))

;; (drop n s) returns a lazy list which makes problems later? FIXME (but this works)
(defn n-drop [n s]
  (if (or (<= n 0) (empty? s))
    s
    (recur (dec n) (pop s))))

(defn println-color [col & strings]
  (println (in-color col (apply str strings))))

(defmacro safe-fn-UNSAFE [f & args]
  `(~f ~@args))
(defmacro safe-fn [f & args]
  `(try
     (~f ~@args)
     (catch Exception e# (println-color :red "Error: " e#))))

(defn func2
  "Takes a binary function and returns a stacker-function which expects two
  items on the stack, performs the function on it and pushes the result back on
  the stack. In other words: it makes a ( a b -- (f b a) )"
  [f]
  (fn [s env]
    (stats-inc :func2)
    (let [
          a (first s) s (rest s)
          b (first s) s (rest s)]
      [(conj s (safe-fn f b a)) env])))

(defn func1
  "Takes a unary function and returns a stacker-function which expects one
  item on the stack, performs the function on it and pushes the result back on
  the stack. In other words: it makes a ( a -- (f a) )"
  ([f]
   (fn [s env]
     (stats-inc :func1-1)
     (let [a (first s) s (rest s)]
       [(conj s (safe-fn f a)) env])))
  ([env f]
   (fn [s env]
     (stats-inc :func1-2)
     (let [a (first s) s (rest s)]
       [(conj s (safe-fn f env a)) env]))))

(defn func1-env
  "Takes a binary function and returns a stacker-function which expects one
  item on the stack, performs the function on it (also adds the env) and pushes the result back on
  the stack. In other words: it makes a ( a -- (f a env) )"
  [f]
  (fn [s env]
    (stats-inc :func1-env)
    (let [a (first s) s (rest s)]
      [(conj s (safe-fn f a env)) env])))

(defn func0
  "Simple push-value-generator."
  [v]
  (fn [s env]
    (stats-inc :func0)
    [(conj s v) env]))

(defn func3
  "Takes a ternary function and returns a stacker-function which expects two
  items on the stack, performs the function on it and pushes the result back on
  the stack. In other words: it makes a ( a b c -- (f c b a) )"
  [f]
  (fn [s env]
    (stats-inc :func3)
    (let [a (first s) s (rest s)
          b (first s) s (rest s)
          c (first s) s (rest s)]
      [(conj s (safe-fn f c b a)) env])))


;; All stacker functions take a stack and an env and return a stack and an env

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

(defn apply-sfun-list
  [stack-env sfuns]
  ;; (stats-inc :apply-sfun-list)
  (if (empty? sfuns)
    stack-env
    (let [[stack env] stack-env]
      (recur ((first sfuns) stack env) (rest sfuns)))))

(defn compile-tokens
  "compiles tokens and returns a list [x x x] of functions which
  each takes a stack and an env and returns a possible update [stack env] pair."
  [tokens]
  ;; (println "compiling" tokens (empty? tokens))
  (stats-inc :compile-tokens)
  (loop [compiled [] ;; empty = identity
         tokens tokens]
    (stats-inc :compile-tokens-loop)
    (if (empty? tokens)
      compiled;; empty = identity
      (let [[kind value]  (first tokens)
            remaining     (rest tokens)]
        ;; (println "kind " kind " value " value " type" (type value))
        (condp = kind
          :word (let [f (fn [s e]
                          (stats-inc value)
                          (if-let [w (get e value)]
                            (if-let [f (:fn w)] ;; must be a compiled function
                              (let [x (f s e)]
                                (if (:mod-env w)
                                  ;; function can modify the environment:
                                  x ;; this is a new stack and possibly new env
                                  ;; function should not modify the environment:
                                  [(first x) e])) ;; new stack, original unchanged environment
                              [(conj s value) e]
                              #_(do
                                  (println "Error: value not an executable word:" value "=" w)
                                  [s e]))
                            ;; else it is an unknown word
                            (do
                              (if (:-strict e)
                                (do
                                  (println "*** Word not defined:" value)
                                  [s e])
                                ;; else push value on stack
                                [(conj s value) e]))))]
                  (recur (conj compiled f) remaining))
          :quotation (let [q (rest value) ; value = [:S [:word ..] []...]
                           sfuns (compile-tokens q)
                           f (fn [s e] (stats-inc :in-quotation)
                               (apply-sfun-list [s e] sfuns))]
                       (recur (conj compiled (func0 {:fn f})) remaining))
          :sexp (let [v (eval (read-string value))]
                  (if (fn? v) ;; assume it is already a stacker function
                    (recur (conj compiled v) remaining)
                    ;; else it is a value which is pushed on the stack
                    (recur (conj compiled (func0 v)) remaining)))
          :reader (recur (conj compiled (func0 (read-string value))) remaining)
          :keyword (recur (conj compiled (func0 (keyword value))) remaining)
          :str (recur (conj compiled (func0 (read-string value))) remaining))))))

(defn apply-tokens
  [stack env tokens]
  (stats-inc :apply-tokens)
  (let [sfuns (compile-tokens tokens)]
    (apply-sfun-list [stack env] sfuns)))

(defn quotation-to-fn [q] ;; TODO: rename sfuns-to-fn
  (stats-inc :quotation-to-fn)
  (:fn q))

(defn string-to-tokens
  "Parses the string s into tokens."
  [str]
  (stats-inc :string-to-tokens)
  (let [p (parser str)]
    ;;(pp/pprint p)
    (when-let [tokens (rest p)]
      ;;(pp/pprint tokens)
      tokens)))

(defn compile-string-to-sfun [s]
  (stats-inc :compile-string-to-sfun)
  (let [sfuns (compile-tokens (string-to-tokens s))]
    (fn [s e]
      (apply-sfun-list [s e] sfuns))))

(defn compile-quotation [q]
  (if-let [src (:src q)]
    (assoc q :fn (compile-string-to-sfun src))
    q))

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
              :takes 1 :leaves 0
              :doc "Pops the top of the stack and displays it."
              :fn (fn [s env]
                    (println (peek s))
                    [(rest s) env])}
         "+" {:signature "(n1 n2 -- n3)"
              :takes 2 :leaves 1
              :fn (func2 +)}
         "++" {:signature "(id -- n)"
               :takes 1 :leaves 1
               :doc "increments the environment id and returns the new value on the stack."
               :test [["5 :x set :x ++" "6"]]
               :mod-env true
               :fn (fn [s env]
                     (let [id (first s) s (rest s)
                           val (inc (or (get env id) 0))]
                       [(conj s val) (assoc env id val)]))}
         "-" {:signature "(n1 n2 -- n3)"
              :takes 2 :leaves 1
              :fn (func2 -)}
         "--" {:signature "(id -- n)"
               :takes 1 :leaves 1
               :doc "decrements the environment id and returns the new value on the stack."
               :test [["5 :x set :x --" "4"]]
               :mod-env true
               :fn (fn [s env]
                     (let [id (first s) s (rest s)
                           val (dec (or (get env id) 0))]
                       [(conj s val) (assoc env id val)]))}
         "*" {:signature "(n1 n2 -- n3)"
              :takes 2 :leaves 1
              :fn (func2 *)}
         "/" {:signature "(n1 n2 -- n3)"
              :takes 2 :leaves 1
              :fn (func2 /)}
         ">" {:signature "(n1 n2 -- bool)"
              :takes 2 :leaves 1
              :fn (func2 >)}
         ">=" {:signature "(n1 n2 -- bool)"
               :takes 2 :leaves 1
               :test [["3 -3 >= [1] [0] if" "1"]]
               :fn (func2 >=)}
         "<=" {:signature "(n1 n2 -- bool)"
               :takes 2 :leaves 1
               :test [["-3 -3 <= [1] [0] if" "1"]]
               :fn (func2 <=)}
         "<" {:signature "(n1 n2 -- bool)"
              :takes 2 :leaves 1
              :test [["-3 -1 < [1] [0] if" "1"]]
              :fn (func2 <)}
         "=" {:signature "(a b -- bool)"
              :takes 2 :leaves 1
              :fn (func2 =)}
         "and" {:signature "(bool-1 bool-2 -- bool-3)"
                :takes 2 :leaves 1
                :test [["true true and" "true"]
                       ["true false and" "false"]
                       ["false true and" "false"]
                       ["false false and" "false"]]
                :fn (func2 (fn [a b] (and a b)))}
         "or" {:signature "(bool-1 bool-2 -- bool-3)"
               :takes 2 :leaves 1
               :test [["true true or" "true"]
                      ["true false or" "true"]
                      ["false true or" "true"]
                      ["false false or" "false"]]
               :fn (func2 (fn [a b] (or a b)))}
         "not" {:signature "(bool -- bool)"
                :takes 1 :leaves 1
                :doc "negates the top of the stack."
                :test [["false not" "true"]
                       ["true not" "false"]]
                :fn (func1 not)}
         "clear" {:signature "(? -- )"
                  :doc "Clears the stack completely."
                  :fn (fn [s env] [() env])}
         "inc" {:signature "(n1 -- n2)"
                :takes 1 :leaves 1
                :test [["4 inc" "5"]]
                :fn (func1 inc)}
         "dec" {:signature "(n1 -- n2)"
                :takes 1 :leaves 1
                :test [[ "4 dec" "3"]]
                :fn (func1 dec)}
         "compile" {:signature "(q -- q)"
                    :takes 1 :leaves 1
                    :doc "Compiles the quotation on the top of the stack. Multiple applications are possible, but meaningless"
                    :test [["\"22 7 /\" parse compile apply" "22 7 /"]
                           ["\"dup *\" parse compile \"sqr\" set 4 sqr" "16"]
                           ["\"42\" parse compile compile compile apply" "42"]]
                    :fn (func1-env
                         (fn [q env]
                           (compile-quotation q)))}
         "parse" {:signature "(str -- q)"
                  :takes 1 :leaves 1
                  :doc "parses the string str and leaves the result as a compiled quotation on the stack."
                  :test [["\"22 7 /\" parse apply" "22 7 /"]
                         ["\"dup *\" parse \"sqr\" set 4 sqr" "16"]]
                  :fn (fn [s env]
                        (let [strg (first s) s (rest s)]
                          [(conj s {:src strg :fn (compile-string-to-sfun strg)}) env]))}
         "join" {:signature "(seq str-delim -- str-joined)"
                 :takes 2 :leaves 1
                 :doc "joins the elements of seq with str in between."
                 :test [["1 5 range \", \" join" "\"1, 2, 3, 4, 5\""]
                        ["1 8 range nil join"  " \"12345678\" "]]
                 :fn (func2 (fn [seq s]
                              (str/join s seq)))}
         "n-param" {:signature "(n -- )"
                    :takes 1 :leaves 0
                    :doc "takes the n next items from the stack and converts them into environment variables, :a, :b, ..."
                    :test [["10 20 30 3 n-param :c get :a get +" "40"]]
                    :mod-env true
                    :fn (fn [s env]
                          (let [n (first s) s (rest s)]
                            (if (<= 1 n 26)
                              (loop [s s i n env env]
                                (if (> i 0)
                                  (let [v (first s) s (rest s)]
                                    (recur s (dec i) (assoc env (keyword (str (char (+ i -1 (int \a))))) v)))
                                  [s env]))
                              (do
                                (println "*** number of params should be 1 <= n <= 26")
                                [s env]))))}
         "param" {:signature "([a b...] -- )"
                  :takes 1 :leaves 0
                  :doc "takes the linear quotation (should contain only ids) from the stack and saves the corresponding stack items."
                  :test [["10 20 30 [:a _ :c] param :c get :a get +" "40"]
                         ["10 20 [a b] param b get a get +" "30"]
                         ["990 10 [:x] param :x get +" "1000"]]
                  :mod-env true
                  :fn (fn [s env]
                        (let [q (first s) s (rest s)
                              ;; q (assoc q :mod-env true)
                              param-fn (quotation-to-fn q)
                              [param-names _] (param-fn () env)]
                          (loop [s s
                                 env env
                                 seq param-names]
                            (if (empty? seq)
                              [s env]
                              (let [v (first s) s (rest s)
                                    id (first seq)]
                                (recur s (assoc env id v) #_(if (= id "_")
                                                              env ;; skip this
                                                              (assoc env id v))
                                       (pop seq)))))))}
         "grab" {:signature "(a b c n -- [a b c])"
                 :leaves 1
                 :doc "grabs the top n elements and makes them a sequence which is pushed back on the stack."
                 :fn (fn [s env]
                       (let [n (first s) s (rest s)]
                         [(conj (n-drop n s) (reverse (take n s))) env]))}
         "count-old" (compile-quotation
                      {:signature "(seq -- a)"
                       :takes 1 :leaves 1
                       :doc "counts the number of elements in the sequence."
                       :test [["2 19 inc inc range count-old" "20"]]
                       :src "[1] map [+] reduce"
                       })
         "count" {:signature "([a b...] -- n)"
                  :takes 1 :leaves 1
                  :doc "counts the sequence on top and puts the count (only) on the stack."
                  :test [["2 19 inc inc range count" "20"]]
                  :fn (func1 (fn [sq] (count (seq sq))))
                  }
         "test" {:signature "(id -- bool)"
                 :takes 1 :leaves 1
                 :doc "Performs a self-test (if defined) on the word. Leaves the result of the tests on the stack."
                 :fn (func1-env (fn [id env]
                                  (let [tests (:test (get env id))]
                                    (reduce (fn [a b] (and a b)) true
                                            (for [[test result check] tests]
                                              (do-test id test result check env))))))}
         "if" {:signature "(bool q-true q-false -- ?)"
               :takes 3
               :test [["4 5 > [ :yes ] [ :no ] if" ":no"]]
               :doc "if bool is true, apply q-true, otherwise apply q-false. "
               :fn (fn [s env]
                     (let [else (first s) s (rest s)
                           when (first s) s (rest s)
                           chck (first s) s (rest s)
                           f (quotation-to-fn (if chck when else))]
                       (f s env)))}
         "doc" {:signature "(id -- )"
                :takes 1 :leaves 0
                :fn (fn [s env]
                      (let [id (first s) s (rest s)
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
              :takes 1 :leaves 1 :same-value true ;; side effect only
              :fn (fn [s env]
                    (println (first s))
                    [s env])}
         "get" {:signature "(a -- b)"
                :takes 1 :leaves 1
                :fn (fn [s env]
                      (let [id (first s) s (rest s)]
                        [(conj s (get env id)) env]))}
         "set" {:signature "(a-value id -- )"
                :takes 2 :leaves 0
                :doc "sets the entry named id to a-value in the env. a-value can be a quotation."
                :mod-env true
                :test [["113. :r set 355 :r get /" "3.1415929203539825"]
                       ["-1 :a set [5 :a set [99 :a set] apply :a get dup *] apply :a get" "25 -1"]
                       ["[5] a set a [6] \"a\" set a +" "11"]
                       ["[[:ok] \"get-ok\" set get-ok] apply" ":ok"]]
                :fn (fn [s env]
                      (let [id (first s) s (rest s)
                            v (first s) s (rest s)]
                        [s (assoc env id v)]))}
         "apply" {:signature "(q -- ?)"
                  :takes 1
                  :test [["-1 4 5 [+] apply" "-1 9"]]
                  :fn (fn [s env]
                        (let [q (first s) s (rest s)
                              f (quotation-to-fn q)]
                          (f s env)))}
         "until" {:signature "(q-body q-test -- ?)"
                  :takes 2
                  :doc "applies q-body on the stack repeatedly until q-test returns true. "
                  :test [["4 [inc] [dup 10 >=] until" "10"]]
                  :fn (fn [s env]
                        (let [q-test (first s) s (rest s)
                              f-test (quotation-to-fn q-test)
                              q-body (first s) s (rest s)
                              f-body (quotation-to-fn q-body)]
                          ;; the env is reused during while, but reset afterwards
                          (loop [s s env-while env]
                            (let [[s env-while] (f-test s env-while)
                                  result (first s) s (rest s)]
                              (if-not result
                                (let [[s env-while] (f-body s env-while)]
                                  (recur s env-while))
                                [s env])))))}
         "while" {:signature "(q-body q-test -- ?)"
                  :takes 2
                  :doc "as long as the q-test returns true, apply q-body."
                  :test [["4 [inc] [dup 10 >=] until" "10"]]
                  :fn (fn [s env]
                        (let [q-test (first s) s (rest s)
                              f-test (quotation-to-fn q-test)
                              q-body (first s) s (rest s)
                              f-body (quotation-to-fn q-body)]
                          ;; the env is reused during while, but reset afterwards
                          (loop [s s env-while env]
                            (let [[s env-while] (f-test s env-while)
                                  result (first s) s (rest s)]
                              (if result
                                (let [[s env-while] (f-body s env-while)]
                                  (recur s env-while))
                                [s env])))))}
         "stats" {:signature "( -- )"
                  :takes 0 :leaves 0
                  :fn (fn [s e]
                        (when @stats
                          (let [seconds (/ (double (- (. System (nanoTime)) (get @stats :nano-time-start))) 1000000000.0)]
                            (swap! stats #(dissoc % :nano-time-start))
                            (pp/pprint (sort #(< (second %1) (second %2)) @stats))
                            (println "Time stats:" seconds "secs")))
                        [s e])}
         "stats-on" {:signature "( -- )"
                     :takes 0 :leaves 0
                     :fn (fn [s e] (reset! stats {:nano-time-start (. System (nanoTime))}) [s e])}
         "stats-off" {:signature "( -- )"
                      :takes 0 :leaves 0
                      :fn (fn [s e]
                            (println "Time since stats-on:"  (/ (double (- (. System (nanoTime)) (get @stats :nano-time-start))) 1000000000.0) "secs")
                            (reset! stats nil)
                            [s e])}
         "do"    {:signature "(seq -- seq)"
                  :takes 1 :leaves 1
                  :doc "realizes a potential lazy sequence"
                  :fn (func1 doall)}
         "slurp" {:takes 1 :leaves 1
                  :doc "Takes the filename from the stack and reads it as a string. Leaves the string on the stack."
                  :fn (func1 (fn [file]
                               (slurp file)))}
         "spit"  {:takes 2 :leaves 1
                  :doc "Takes the string and a filename and writes the string to filename."
                  :fn (func2 (fn [contents file]
                               (spit file contents)))}
         "split" {:takes 2 :leaves 1
                  :doc "takes a string and a delimiter (re) from the stack and splits the string accordingly."
                  :fn (func2 str/split)}
         "split-lines" {:takes 1 :leaves 1
                        :doc "takes a string and splits it into lines."
                        :fn (func1 str/split-lines)}
         "load"  (compile-quotation
                  {:signature "(f -- ?)"
                   :doc "loads the filename f, parse and applies the contents."
                   :mod-env true
                   :src "slurp parse apply"
                   })
         "range" {:signature "(n1 n2 -- seq)"
                  :takes 2 :leaves 1
                  :doc "returns a lazy sequence from n1..n2 (note: including both n1 and n2). If n2<n1 the sequence is reversed."
                  :test [["5 3 range [+] reduce" "12"]]
                  :fn (func2 (fn [a b]
                               (if (<= a b)
                                 (range a (inc b))
                                 (range a (dec b) -1))))}
         "drop"  {:signature "(a -- )"
                  :takes 1 :leaves 0
                  :fn (fn [s e] [(rest s) e])}
         "skip"  {:signature "(seq skip-num -- seq)"
                  :takes 2 :leaves 1
                  :doc "skips the first skip-num elements from the sequence."
                  :test [["1 20 range 18 skip [+] reduce" "39"]]
                  :fn (func2 (fn [sequence skip-num]
                               (drop skip-num (seq sequence))))}
         "take"  {:signature "(seq num -- seq)"
                  :takes 2 :leaves 1
                  :doc "takes the first num elements from the sequence."
                  :fn (func2 (fn [sequence take-num]
                               (take take-num (seq sequence))))}
         "reverse"  {:signature "(seq -- seq)"
                     :takes 1 :leaves 1
                     :doc "reverses the sequence."
                     :fn (func1 (fn [sequence]
                                  (reverse (seq sequence))))}
         "reduce" {:signature "(seq1 q -- a)"
                   :takes 1 :leaves 1
                   :doc "reduces the sequence using the quotation q and leaves the result on the stack."
                   :test [["1 10 range [*] reduce" "3628800"]]
                   :fn (fn [s env]
                         (let [reduce-q (first s) s (rest s)
                               f (quotation-to-fn reduce-q)
                               sequence (first s) s (rest s)
                               sequence (seq sequence)]
                           [(conj s
                                  (reduce (fn [a b]
                                            ;; will throw away interim envs
                                            (first (first (f (conj s a b) env)))) sequence)) env]))}
         "map"   {:signature "(seq1 q -- seq2)"
                  :takes 2 :leaves 1
                  :fn (fn [s env]
                        (let [map-q (first s) s (rest s)
                              f (quotation-to-fn map-q)
                              sequence (first s) s (rest s)]
                          [(conj s
                                 ;; will throw away interim envs
                                 (map #(first (first (f (conj s %) env))) (seq sequence))) env]
                          ))}
         "peek"  {:signature "(a -- a)"
                  :takes 1 :leaves 1 :same-value true
                  :fn (fn [s env]
                        (let [v (first s)]
                          (println v "=" (type v)))
                        [s env])}
         "dup"   {:signature "(a -- a a)"
                  :takes 1 :leaves 2
                  :test [["7 dup" "7 7"]]
                  :doc "Duplicates the top of the stack."
                  :fn (fn [s env] [(conj s (first s)) env])}
         "stack" {:signature "( -- )"
                  :takes 0 :leaves 0
                  :fn (fn [s env]
                        (print-stack s)
                        [s env])}
         "env"   {:signature "( -- )"
                  :takes 0 :leaves 0
                  :fn (fn [s env]
                        [(conj s (sort (map str (keys env)))) env])}
         "exit"  {:signature "( -- )"
                  :takes 0 :leaves 0
                  :fn exit}
         "q"     {:signature "( -- )"
                  :takes 0 :leaves 0
                  :fn exit}
         "swap" {:signature "(a b -- b a)"
                 :takes 2 :leaves 2
                 :test [["1 2 3 swap" "1 3 2"]
                        ["7 swap" "7"]]
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

(defn make-engine
  ([]          (make-engine nil nil))
  ([env]       (make-engine nil env))
  ([stack env] (agent [(or stack ()) (or env @default-env)])))

(defn perform [[stack env] str]
  (let [tokens (string-to-tokens str)]
    (apply-tokens stack env tokens)))

(defn feed-engine [engine str]
  (send engine perform str))

(defn feed-engine-seq [engine args]
  (feed-engine engine (str/join " " args)))

(defn feed-engine-multi [engine & args]
  (doseq [s args]
    (feed-engine engine s)))

(defn push-stack [engine val]
  (send engine (fn [[stack env] v]
                 [(conj stack v) env]) val))

(defn get-stack [engine]
  (await engine)
  (first @engine))

(defn peek-stack [engine]
  (first (get-stack engine)))

(defn engine-set-word [engine index word-def]
  (send engine
        (fn [[stack env] id v]
          [stack (assoc env id v) env])
        index word-def)
  (await engine))

(defn repl [engine]
  (let [lr (make-line-reader)]
    (loop []
      (let [r (.readLine lr prompt)]
        ;; (println r)
        (when (not= r "bye")
          (feed-engine engine r)
          (await engine)
          (recur))))
    engine))

(defn -main [& args]
  (let [engine (make-engine)]
    (feed-engine-seq engine args)
    (await engine)
    (repl engine)
    (let [[s _] @engine
          top (first s)]
      (when (number? top)
        (System/exit top)))))
