(ns
  ^{:doc "Stacker - stack based repl interface -- similar to Forth/Factor"
    :author "Jörg Ramb, 2017"}
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
    )
  )

(def prompt "\u001B[32m>\u001B[0m ")

(defn spop [s]
  [(pop s) (peek s)])

(defn func2
  "Takes a binary function and returns a stacker-function which expects two
  items on the stack, performs the function on it and pushes the result back on
  the stack. In other words: it makes a ( a b -- (f b a) )"
  [f]
  (fn [s env]
    (let [[s a] (spop s)
          [s b] (spop s)]
      [(conj s (f b a)) env])))

(defn func1
  "Takes a unary function and returns a stacker-function which expects one
  items on the stack, performs the function on it and pushes the result back on
  the stack. In other words: it makes a ( a -- (f a) )"
  [f]
  (fn [s env]
    (let [[s a] (spop s)]
      [(conj s (f a)) env])))

(defn func3
  "Takes a ternary function and returns a stacker-function which expects two
  items on the stack, performs the function on it and pushes the result back on
  the stack. In other words: it makes a ( a b c -- (f c b a) )"
  [f]
  (fn [s env]
    (let [[s a] (spop s)
          [s b] (spop s)
          [s c] (spop s)]
      [(conj s (f c b a)) env])))

(defn env-neutral-function
  "Takes a function (s->s) and returns a stacker-function which does not change the env."
  [f]
  (fn [s env]
    [(f s) env]))

(defn top-if [stack cond else]
  (let [t (peek stack)]
    (if (cond t) t else)))


;; All stacker functions take a stack and an env and return a stack and an env
(defn sf-drop [s env]
  [(if (first s) (pop s) s) env])

(defn print-stack [stack]
  (println "S:" (str/join " " (reverse stack))))

(defn print-env [env]
  (println "E:" (sort (map str (keys env)))))

(def parser
  (instaparse/parser
   "S = (blank|word|str|keyword|num|quotation)*
    <blank> = <#'\\s+'>
    quotation = <'['> S <']'>
    num = #'-?[0-9]+\\.?[0-9]*[M]?'
    str = <'\"'> #'[^\"]*' <'\"'>
    keyword = <':'> #'[^\\s\\]]*'
    word = #'[^0-9\\s\"][^\\s\\]]*'
"))

;; (parser "A hej \"hej\" [ ]");
;; (pp/pprint (parser "dup 3.4 3.4 3.4 dip \"hwody bowdy\" and"));
(defn apply-tokens
  "Applies the tokens on the [stack env] and returns a new [stack env] when done"
  [[stack env] tokens]
  ;; (pp/pprint tokens)
  ;; (pp/pprint stack)
  (if (empty? tokens)
    [stack env] ;; return what is left
    (let [[kind value]  (first tokens)
          remaining     (rest tokens)]
      ;(println "kind " kind " value " value " type" (type value))
      (condp = kind
        :word (let [f (:fn (get env value))]
                (if (nil? f)
                  (do
                    (println "*** Word not defined:" value "(Aborting execution)")
                    [stack env])
                  (recur (f stack env) remaining)))
        :quotation (let [[_ v] value]
                     (recur [(conj stack {:quotation (rest value)}) env] remaining))
        :num (let [v (read-string value)]
               (recur [(conj stack v) env] remaining))
        :keyword (recur [(conj stack (keyword value)) env] remaining)
        :str (recur [(conj stack value) env] remaining)))))

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
         :fn (fn [s env]
               (println (peek s))
               (sf-drop s env))}
    "+" {:signature "(n1 n2 -- n3)"
         :fn (func2 +)}
    "-" {:signature "(n1 n2 -- n3)"
         :fn (func2 -)}
    "*" {:signature "(n1 n2 -- n3)"
         :fn (func2 *)}
    "/" {:signature "(n1 n2 -- n3)"
         :fn (func2 /)}
    ">" {:signature "(n1 n2 -- bool)"
         :fn (func2 >)}
    ">=" {:signature "(n1 n2 -- bool)"
          :fn (func2 >=)}
    "<=" {:signature "(n1 n2 -- bool)"
          :fn (func2 <=)}
    "<" {:signature "(n1 n2 -- bool)"
         :fn (func2 <)}
    "=" {:signature "(a b -- bool)"
         :fn (func2 =)}
    "and" {:signature "(bool-1 bool-2 -- bool-3)"
          :fn (func2 (fn [a b] (and a b)))}
    "or" {:signature "(bool-1 bool-2 -- bool-3)"
          :fn (func2 (fn [a b] (or a b)))}
    "inc" {:signature "(n1 -- n2)"
           :test [["4 inc" "5"]]
           :fn (func1 inc)}
    "dec" {:signature "(n1 -- n2)"
           :test [[ "4 dec" "3"]]
           :fn (func1 dec)}
    "parse" {:signature "str -- q"
             :doc "parses the string str and leaves the result as a quotation on the stack."
             :fn (fn [s env]
                   (let [[s string] (spop s)]
                     [(conj s {:quotation (string-to-tokens string)}) env]))}
    "test" {:signature "(id -- bool)"
            :doc "Performs a self-test (if defined) on the word."
            :fn (fn [s env]
                  (let [[s id] (spop s)
                        [test result check] (first (:test (get env id)))]
                    [(conj s (if test
                               (let [[s2 env] (apply-tokens [() env] (string-to-tokens test))
                                     after-test s2
                                     [s3 env] (apply-tokens [() env] (string-to-tokens result))
                                     ok? (= s2 s3)]
                                 (println test "-->" s2 "expected" s3 ":" (if ok? "PASS" "FAIL"))
                                 ok?)
                               (do
                                 (println "No test defined for" id ": SKIP")
                                 true))) env]))}
    "if" {:signature "(bool q-true q-false -- ?)"
          :test [["4 5 > [ :yes ] [ :no ] if" ":no"]]
          :doc "if bool is true, apply q-true, otherwise apply q-false. "
          :fn (fn [s env]
                (let [[s else] (spop s)
                      [s when] (spop s)
                      [s chck] (spop s)]
                  (apply-tokens [s env] (:quotation (if chck when else)))))}
    "doc" {:signature "(id -- )"
            :fn (fn [s env]
                  (let [[s id] (spop s)
                        e (get env id)]
                    (if e
                      (do
                        (println "### " id "--" (:signature e))
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
    "put" {:signature "(a id -- )"
           :fn (fn [s env]
                 (let [[s id] (spop s)
                       [s v] (spop s)]
                   [s (assoc env id v)]))}
    "apply" {:signature "(q -- ?)"
             :fn (fn [s env]
                   (let [[s tokens] (spop s)]
                     (apply-tokens [s env] (:quotation tokens))))}
    "do"    {:signature "(seq -- seq)"
             :doc "realizes a potential lazy sequence"
             :fn (func1 doall)}
    "range" {:signature "(n1 n2 -- seq)"
             :doc "returns a lazy sequence from n1..n2 (note: including both n1 and n2). If n2<n1 the sequence is reversed."
             :fn (func2 (fn [a b]
                          (if (<= a b)
                            (range a (inc b))
                            (range a (dec b) -1))))}
    "drop"  {:signature "(a -- )"
             :fn sf-drop}
    "reduce" {:signature "(seq1 q -- a)"
              :doc "reduces the sequence using the quotation q and leaves the result on the stack."
              :test [["1 10 range [*] reduce" "3628800"]]
              :fn (fn [s env]
                    (let [[s reduce-fn] (spop s)
                          reduce-fn (:quotation reduce-fn)
                          [s sequence] (spop s)]
                      [(conj s
                             (reduce (fn [a b]
                                       (first (first (apply-tokens [(conj s a b) env] reduce-fn)))) sequence)) env]
                      ))}
    "map"   {:signature "(seq1 q -- seq2)"
             :fn (fn [s env]
                   (let [[s map-fn] (spop s)
                         [s sequence] (spop s)]
                     [(conj s
                            (map #(first (first (apply-tokens [(conj s %) env] (:quotation map-fn)))) sequence)) env]
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
        initial (apply-tokens [start-stack env] start-tokens)]
    (loop [[stack env] initial]
      ;;(print-stack stack)
      ;; (print-env env)
      (let [r (.readLine lr prompt)
            tokens (string-to-tokens r)]
        (if (or (not tokens) (= r "bye"))
          [stack env] ;; return the stack and current env
          (recur (apply-tokens [stack env] tokens)))))))


(defn -main [& args]
  #_(when (not (nil? args))
    (println "args: " args))
  (let [[s env] (repl () @default-env args)
        top (first s)]
    (when (number? top)
      (System/exit top))))

