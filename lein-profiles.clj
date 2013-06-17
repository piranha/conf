{:user {:plugins [[lein-shegon "0.1.1"]
                  ;[lein-outdated "1.0.0"]
                  [lein-ancient "0.2.0"]
                  [jonase/eastwood "0.0.2"]]
        :injections [(require '[clojure.string :as s])
                     (defn parse-qs [a] (->> a
                                             (#(s/split % #"&"))
                                             (map #(s/split % #"="))
                                             (map (fn [[k v]] [(keyword k) (java.net.URLDecoder/decode v)]))
                                             (into {})))]}}
