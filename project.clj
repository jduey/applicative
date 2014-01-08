(defproject net.clojure/applicative "1.0.2"
  :description "A protocol based implementation of applicative functors"
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :repositories ~(some #(try (load-file (str % "repositories.clj")) (catch Exception e)) ["../../" ""]))
