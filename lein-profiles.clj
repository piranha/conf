{:user {:plugins [[lein-ancient "0.6.14"]
                  [com.jakemccrary/lein-test-refresh "0.15.0"]
                  [nightlight/lein-nightlight "2.1.0"]]
        :dependencies [[criterium "0.4.3"]
                       [vvvvalvalval/scope-capture "0.3.1"]
                       [pjstadig/humane-test-output "0.8.3"]]
        :injections [(require 'sc.api)
                     (require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)

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
