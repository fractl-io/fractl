{:name :Library
 :agentlang-version "current"
 :components [:Test.Sample/Library/Identity
              :Test.Sample/Library/Catalog
              :Test.Sample/Library/Ledger]
 :config {:service {:port 8000}
          :zero-trust-rbac false
          :store {:type :mem}}}
