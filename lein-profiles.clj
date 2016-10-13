{:user {:plugins [[lein-ancient "0.6.10"]
                  [jonase/eastwood "0.1.4"]
                  [lein-try "0.4.1"]
                  [lein-pdo "0.1.1"]
                  [mvxcvi/whidbey "0.3.2"]
                  [lein-license "0.1.6"]]
        :dependencies [[spyscope "0.1.4" ]
                       [debug-repl "0.3.2"]
                       [criterium "0.4.3"]
                       #_[inspector-jay "0.3"]]
        :injections [(require 'spyscope.core)
                     (require '[alex-and-georges.debug-repl :refer [debug-repl]])
                     (require '[clojure.string :as s])
                     (require '[criterium.core :refer [quick-bench]])

                     (defn parse-qs [a]
                       (->> a
                            (#(s/split % #"&"))
                            (map #(s/split % #"="))
                            (map (fn [[k v]]
                                   [(keyword k)
                                    (if v (java.net.URLDecoder/decode v))]))
                            (into {})))

                     (defn unparse-qs [qs]
                       (->> qs
                            (map #(vec [(name (first %))
                                        (java.net.URLEncoder/encode (str (second %)))]))
                            (map #(s/join "=" %))
                            (s/join "&")))

                     (defmacro dbg [x]
                       `(let [x# ~x]
                          (printf "dbg %s:%s> %s is %s\n"
                                  ~*ns*
                                  ~(:line (meta &form))
                                  ~(pr-str x)
                                  (with-out-str
                                    (clojure.pprint/pprint x#)))
                          (flush)
                          x#))]}}
