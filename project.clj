(defproject net.nogui/stacker "0.3.1-SNAPSHOT"
  :description "Stacker - A stack based language and REPL"
  :author "Jörg Ramb"
  :url "https://github.com/jramb/stacker"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [instaparse "1.4.9"]
                 [jline/jline "2.14.5"]
                 ;; [jline/jline "3.0.0.M1"]
                 ;[org.jline/jline "3.3.0"]
                 ]
  :main net.nogui.stacker
  ;; :main ^:skip-aot net.nogui.stacker
  :profiles {:uberjar {:aot :all}}
  ;; :aot :all
  )
