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

(def prompt "\u001B[32m>\u001B[0m ")

(defn spop [s]
  (if (empty? s)
    [s nil]
    [(pop s) (peek s)]))

(defmacro safe-fn [f & args]
  `(try
     (~f ~@args)
     (catch Exception e# (println (str "Error: " e#)))))

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
  items on the stack, performs the function on it and pushes the result back on
  the stack. In other words: it makes a ( a -- (f a) )"
  [f]
  (fn [s env]
    (let [[s a] (spop s)]
      [(conj s (safe-fn f a)) env])))

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

(defn env-neutral-function
  "Takes a function (s->s) and returns a stacker-function which does not change the env."
  [f]
  (fn [s env]
    [(f s) env]))

(defn top-if [stack condition else]
  (let [t (peek stack)]
    (if (condition t) t else)))


;; All stacker functions take a stack and an env and return a stack and an env
(defn safe-drop [s env]
  [(if (peek s) (pop s) s) env])

(defn safe-print [stack-item]
  ;; FIXME: make this better
  (str stack-item))

(defn print-stack [stack]
  #_(dorun (map-indexed (fn [i val] (print (str i ": "))
                        (pp/pprint val)) (reverse stack)))
  #_(pp/pprint stack #_(str/join " " (reverse stack)))
  (println "S:" (str/join ", " (map safe-print (reverse stack)))))

(defn print-env [env]
  (println "E:" (sort (map str (keys env)))))

(def parser
  (instaparse/parser ;; EBNF
   "S = (blank|word|str|keyword|reader|sexp|quotation)*
    <blank> = <#'\\s+'>
    quotation = <'['> S <']'>
    word = #'[^0-9\\s)\"][^\\s\\]]*'
    str = #'\"([^\\\\\"]|\\\\.)*\"'
    <number> = #'-?[0-9]+\\.?[0-9]*([eE][-+]?[0-9]+)?[M]?'
    sexp = #'\\([^()]*\\)'
    reader = ( number | 'nil' )
    keyword = <':'> #'[^\\s\\]]*'
"))

;; (parser "(+ (inc 5) \"hej\" 4 5)");
(parser "4.5 5 +")
;; (pp/pprint (parser "dup 3.4 3.4 3.4 dip \"hwody bowdy\" and"));
(defn apply-tokens
  "Applies the tokens on the [stack env] and returns a new [stack env] when done"
  [stack env tokens]
  ;; (pp/pprint tokens)
  ;; (pp/pprint stack)
  (if (empty? tokens)
    [stack env] ;; return what is left
    (let [[kind value]  (first tokens)
          remaining     (rest tokens)]
      ;; (println "kind " kind " value " value " type" (type value))
      (condp = kind
        :word (let [w (get env value)]
                (if-let [f (:fn w)]
                  (let [[stack env] (f stack env)]
                    (recur stack env remaining))
                  (if-let [q (:quotation w)]
                    (let [[stack env] (apply-tokens stack env q)]
                      (recur stack env remaining))
                    (do
                      (if (:-strict env)
                        (do
                          (println "*** Word not defined:" value "(pushed on stack)")
                          [stack env])
                        (recur (conj stack value) env remaining))))))
        :quotation (recur (conj stack {:quotation (rest value)}) env remaining)
        :sexp (let [v (eval (read-string value))]
                (recur (conj stack (if (fn? v) {:fn v} v)) env remaining))
        :reader (let [v (read-string value)]
                  (recur (conj stack v) env remaining))
        :keyword (recur (conj stack (keyword value)) env remaining)
        :str (recur (conj stack (read-string value)) env remaining)))))

(defn string-to-tokens
  "Parses the string s into tokens."
  [str]
  (let [p (parser str)]
    ;;(pp/pprint p)
    (when-let [tokens (rest p)]
      ;;(pp/pprint tokens)
      tokens)))

(def default-env
  (atom
   {"." {:signature "(a -- )"
         :doc "Pops the top of the stack and displays it."
         :fn (fn [s env]
               (println (peek s))
               (safe-drop s env))}
    "+" {:signature "(n1 n2 -- n3)"
         :fn (func2 +)}
    "-" {:signature "(n1 n2 -- n3)"
         :fn (func2 -)}
    "*" {:signature "(n1 n2 -- n3)"
         :fn (func2 *)}
    "/" {:signature "(n1 n2 -- n3)"
         :fn (func2 /)}
    "count" {:signature "(seq -- a)"
             :doc "counts the number of elements in the sequence."
             :test [["2 19 inc inc range count" "19 inc"]]
             :quotation (string-to-tokens "[1] map [+] reduce")}
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
    "clear" {:signature "(? -- )"
             :doc "Clears the stack completely."
             :fn (fn [s env] [() env])}
    "inc" {:signature "(n1 -- n2)"
           :test [["4 inc" "5"]]
           :fn (func1 inc)}
    "dec" {:signature "(n1 -- n2)"
           :test [[ "4 dec" "3"]]
           :fn (func1 dec)}
    "parse" {:signature "(str -- q)"
             :doc "parses the string str and leaves the result as a quotation on the stack."
             :fn (fn [s env]
                   (let [[s string] (spop s)]
                     [(conj s {:quotation (string-to-tokens string)}) env]))}
    "join" {:signature "(seq str-delim -- str-joined)"
            :doc "joins the elements of seq with str in between."
            :fn (func2 (fn [seq s]
                         (str/join s seq)))}
    "test" {:signature "(id -- bool)"
            :doc "Performs a self-test (if defined) on the word."
            :fn (fn [s env]
                  (let [[s id] (spop s)
                        [test result check] (first (:test (get env id)))]
                    [(conj s (if test
                               (let [[s2 env] (apply-tokens () env (string-to-tokens test))
                                     [s3 env] (apply-tokens () env (string-to-tokens result))
                                     ok? (= s2 s3)]
                                 (println (if ok? "PASS" "FAIL") ":" id ":" test "-->" s2 "expected" s3 )
                                 ok?)
                               (do
                                 #_(println "SKIP: No test defined for" id)
                                 true))) env]))}
    "if" {:signature "(bool q-true q-false -- ?)"
          :test [["4 5 > [ :yes ] [ :no ] if" ":no"]]
          :doc "if bool is true, apply q-true, otherwise apply q-false. "
          :fn (fn [s env]
                (let [[s else] (spop s)
                      [s when] (spop s)
                      [s chck] (spop s)]
                  (apply-tokens s env (:quotation (if chck when else)))))}
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
           :fn (fn [s env]
                 (let [[s id] (spop s)
                       [s v] (spop s)]
                   [s (assoc env id v)]))}
    "apply" {:signature "(q -- ?)"
             :fn (fn [s env]
                   (let [[s tokens] (spop s)]
                     (apply-tokens s env (:quotation tokens))))}
    "do"    {:signature "(seq -- seq)"
             :doc "realizes a potential lazy sequence"
             :fn (func1 doall)}
    "load"  {:signature "(f -- ?)"
             :doc "loads the filename f, parse and applies the contents."
             :fn (fn [s env]
                   (let [[s f] (spop s)
                         tokens (string-to-tokens (slurp f))]
                     (apply-tokens s env tokens)))}
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
                    (let [[s reduce-fn] (spop s)
                          reduce-fn (:quotation reduce-fn)
                          [s sequence] (spop s)
                          sequence (seq sequence)]
                      [(conj s
                             (reduce (fn [a b]
                                       (first (first (apply-tokens (conj s a b) env reduce-fn)))) sequence)) env]
                      ))}
    "map"   {:signature "(seq1 q -- seq2)"
             :fn (fn [s env]
                   (let [[s map-fn] (spop s)
                         [s sequence] (spop s)]
                     [(conj s
                            (map #(first (first (apply-tokens (conj s %) env (:quotation map-fn)))) (seq sequence))) env]
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
             :fn (fn [s env] (System/exit (top-if s number? 0)))}
    "q"     {:signature "( -- )"
             :fn (fn [s env] (System/exit (top-if s number? 0)))}
    "swap"  {:signature "(a b -- b a)"
             :fn (fn [s env] (let [[a b & r] s]
                              (if (>= (count s) 2)
                                [(conj (conj r a) b) env]
                                [s env])))}}))



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

