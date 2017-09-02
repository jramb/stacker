(ns
  ^{:doc "Stacker - stack based repl interface -- similar to Forth/Factor"
    :author "Jörg Ramb, 2017"}
  net.nogui.stacker
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

(defn env-neutral-function
  "Takes a function (s->s) and returns a stacker-function which does not change the env."
  [f]
  (fn [s env]
    [(f s) env]))

(defn top-if [stack cond else]
  (let [t (peek stack)]
    (if (cond t) t else)))


(defn benchmark []
  (let [s (range 10000)]
    ()))


;; All stacker functions take a stack and an env and return a stack and an env
(def default-env
  (atom
  {"." (fn [s env]
         (println (peek s))
         [(pop s) env])
   "+"      (func2 +)
   "-"      (func2 -)
   "*"      (func2 *)
   "/"      (func2 /)
   "p" (fn [s env]
         (println (peek s))
         [s env])
   "clear"  (fn [s env] [() env])
   "drop"   (fn [s env] [(pop s) env])
   "dup"    (fn [s env] [(conj s (peek s)) env])
   "env"    (fn [s env] (println (sort (keys env)))
              [s env])
   "q"      (fn [s env] (System/exit (top-if s number? 0)))
   "swap"   (fn [s env] (let [[a b & r] s] (conj (conj r a) b)))}))

(defn make-line-reader []
  (let [cr (ConsoleReader.)]
    (.setPrompt cr prompt)
    (.addCompleter cr (StringsCompleter. (keys @default-env)))
    (.addCompleter cr (FileNameCompleter.))
    cr))

(defn print-stack [stack]
  (println "S: " (str/join " " (reverse stack))))

(def parser
  (instaparse/parser
   "S = (blank|word|str|num)*
    <blank> = <#'\\s+'>
    num = #'-?[0-9]+\\.?[0-9]*'
    str = <'\"'> #'[^\"]*' <'\"'>
    word = #'[^0-9\\s\"][^\\s]*'
"))

(defn apply-tokens [[stack env] tokens]
  ;(pp/pprint tokens)
  ;(pp/pprint stack)
  (if (empty? tokens)
    [stack env]
    (let [[kind value]  (first tokens)
          remaining     (rest tokens)]
      ;(println "kind " kind " value " value " type" (type value))
      (condp = kind
        :word (let [f (get env value)]
                (if (nil? f)
                  (do
                    (println "*** Word not defined:" value "(Aborting execution)")
                    [stack env])
                  (recur (f stack env) remaining)))
        :num (let [v (read-string value)]
               (recur [(conj stack v) env] remaining))
        :str (recur [(conj stack value) env] remaining)))))

;(pp/pprint (parser "dup 3.4 3.4 3.4 dip \"hwody bowdy\" and"))

(defn string-to-tokens [s env lr]
  (let [p (parser s)]
    ;(pp/pprint p)
    (when-let [tokens (rest p)]
      ;(pp/pprint tokens)
      tokens)))


(defn repl [start-words]
  (let [lr (make-line-reader)
        env @default-env
        start-tokens (string-to-tokens (str/join " " start-words) env lr)
        initial (apply-tokens [() env] start-tokens)]
    (loop [[stack env] initial]
      (print-stack stack)
      (let [r (.readLine lr prompt)
            e (string-to-tokens r env lr)]
        (if (or (not e) (= r "bye"))
          stack
          (recur (apply-tokens [stack env] e)))))))


(defn -main [& args]
  (when (not (nil? args))
    (println "args: " args))
  (let [s (repl args)
        top (first s)]
    (when (number? top)
      (System/exit top))))

