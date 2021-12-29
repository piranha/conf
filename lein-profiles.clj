{:user {:plugins      [[com.jakemccrary/lein-test-refresh "0.24.1"]]
        :aliases      {"outdated" ["run" "-m" "antq.core"]}
        :dependencies [[com.github.liquidz/antq "RELEASE"]
                       [hashp "0.1.1"]]
        :injections   [(require 'hashp.core)]}}
