(defproject fractl.example/camel "0.0.1"
  :dependencies [[com.github.fractl-io/fractl "0.5.1"]
                 [org.apache.camel/camel-main "4.6.0-SNAPSHOT"]
                 [org.apache.camel/camel-jackson "4.6.0-SNAPSHOT"]
                 [org.apache.camel/camel-salesforce "4.6.0-SNAPSHOT"]]

  :main fractl.example.camel
  :aot :all)
