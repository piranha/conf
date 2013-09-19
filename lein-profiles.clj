{:user {:plugins [[lein-shegon "0.1.1"]
                  [lein-ancient "0.4.4"]
                  [jonase/eastwood "0.0.2"]
                  [lein-try "0.3.0"]
                  ;[lein-zeroconf "0.1.0"]
                  ;[lein-ritz "0.7.0"]
                  ]
        :dependencies [[spyscope "0.1.3"]
                       [debug-repl "0.3.2"]]
        :injections [;(require 'spyscope.core)
                     (require '[alex-and-georges.debug-repl :refer [debug-repl]])
                     (require '[clojure.string :as s])
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
                           (s/join "&")))]}}
