(ns
  ^{:doc "Stacker - stack based repl interface -- similar to Forth/Factor"
    :author "Jörg Ramb, 2017"}
  net.nogui.stacker
  (:require [clojure.string :as str]
            [instaparse.core :as instaparse]
            [clojure.pprint :as pp])
  (:import
    (java.util LinkedList)
    (jline.console ConsoleReader)
    (jline.console.completer AnsiStringsCompleter)
    (jline.console.completer CandidateListCompletionHandler)
    (jline.console.completer Completer)
    (jline.console.completer FileNameCompleter)
    (jline.console.completer StringsCompleter)
    )
  )

(def prompt "\u001B[32m>\u001B[0m ")

(defn func2 [f]
  (fn [s]
    (let [a (first s)
          b (second s)
          r (rest (rest s))]
      (conj r (f b a)))))

(defn top-if [stack cond else]
  (let [t (first stack)]
    (if (cond t) t else)))

(def default-env
  (atom 
  {"." (fn [s]
          (println (first s))
          (rest s))
   "clear"  (fn [s] ())
   "bye"    (fn [s] (System/exit (top-if s number? 0)))
   "dup"    (fn [s] (conj s (first s)))
   "drop"   (fn [s] (rest s))
   "swap"   (fn [s] (let [[a b & r] s] (conj (conj r a) b)))
   "+"      (func2 +)
   "-"      (func2 -)
   "*"      (func2 *)
   "/"      (func2 /)}))

(defn make-line-reader []
  (let [cr (ConsoleReader.)]
    (.setPrompt cr prompt)
    (.addCompleter cr (StringsCompleter. (keys @default-env)))
    (.addCompleter cr (FileNameCompleter.))
    cr))

(defn print-stack [stack]
  (println "S: " (str/join " " (reverse stack))))

(def parser (instaparse/parser
              "S = (blank|word|str|num)* 
              <blank> = <#'\\s+'>
              num = #'-?[0-9]+\\.?[0-9]*'
              str = <'\"'> #'[^\"]*' <'\"'>
              word = #'[^-0-9\\s\"][^\\s]*' "
              ))

(defn apply-tokens [stack env tokens]
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
                  (println "*** Word not defined:" value)
                  (recur (f stack) env remaining)))
        :num (let [v (read-string value)]
               (recur (conj stack v) env remaining))
        :str (recur (conj stack value) env remaining)))))

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
        initial (apply-tokens () env start-tokens)]
    (loop [[stack env] initial]
      (print-stack stack)
      (let [r (.readLine lr prompt)
            e (string-to-tokens r env lr)]
        (if (or (not e) (= r "bye"))
          stack
          (recur (apply-tokens stack env e)))))))


(defn -main [& args]
  (when (not (nil? args))
    (println "args: " args))
  (let [s (repl args)
        top (first s)]
    (when (number? top)
      (System/exit top))))

