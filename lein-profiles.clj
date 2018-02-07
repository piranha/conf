{:user {:plugins [[lein-ancient "0.6.14"]
                  ;; [lein-kibit "0.1.3"
                  ;;  :exclusions [org.clojure/clojure]]
                  ;; [lein-bikeshed "0.4.1"]
                  ;; [cider/cider-nrepl "0.16.0"]
                  ;; [jonase/eastwood "0.1.4"]
                  ;; [lein-try "0.4.1"]
                  ;; [lein-pdo "0.1.1"]
                  ;; [mvxcvi/whidbey "0.3.2"]
                                        ;[lein-license "0.1.6"]
                  [com.jakemccrary/lein-test-refresh "0.15.0"]
                  [nightlight/lein-nightlight "2.1.0"]
                  ]
        :dependencies [#_[spyscope "0.1.4" ]
                       #_[debug-repl "0.3.2"]
                       #_[inspector-jay "0.3"]
                       [criterium "0.4.3"]
                       [vvvvalvalval/scope-capture "0.1.4"]]
        :injections [(require 'sc.api)

                     (defmacro dbg [x]
                       `(let [x# ~x]
                          (printf "dbg %s:%s> %s is %s\n"
                                  ~*ns*
                                  ~(:line (meta &form))
                                  ~(pr-str x)
                                  (with-out-str
                                    (clojure.pprint/pprint x#)))
                          (flush)
                          x#))]

        :-injections [(require 'spyscope.core)
                      (require '[alex-and-georges.debug-repl :refer [debug-repl]])
                      (require '[criterium.core :refer [quick-bench]])]}}
