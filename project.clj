(defproject stacker "0.1.0-SNAPSHOT"
  :description "Stacker"
  :url "http://jramb.com/stacker"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 ;[org.jline/jline "3.3.0"]  
                 [instaparse "1.4.7"]
                 [jline/jline "2.14.2"]]  
  :main net.nogui.stacker
  :aot :all)
