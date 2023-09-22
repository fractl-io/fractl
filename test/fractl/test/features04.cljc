(ns fractl.test.features04
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.lang
             :refer [component attribute event
                     entity record relationship
                     dataflow]]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest issue-926-rbac-ui-apis
  (let [rbac-spec [{:roles ["user" "manager"] :allow :*}
                   {:roles ["guest"] :allow [:read]}]
        ui-spec {:card {:bgcolor :red}}]
    (defcomponent :I926
      (entity
       :I926/E
       {:Id :Identity
        :X :Int
        :meta {:author "rj"}
        :rbac rbac-spec
        :ui ui-spec}))
    (is (= rbac-spec (cn/fetch-rbac-spec :I926/E)))
    (is (= ui-spec (cn/fetch-ui-spec :I926/E)))))

(deftest issue-980-compilation
  (defcomponent :I980
    (entity
     :I980/A
     {:Id {:type :Int :identity true}
      :X :Int})
    (entity
     :I980/B
     {:Id {:type :Int :identity true}
      :Y :Int})
    (entity
     :I980/C
     {:Id {:type :Int :identity true}
      :Z :Int})
    (relationship
     :I980/R1
     {:meta {:contains [:I980/A :I980/B]}})
    (relationship
     :I980/R2
     {:meta {:between [:I980/B :I980/C]}}))
  (apply dataflow :I980/Temp
         (fractl.compiler/parse-relationship-tree
          {:I980/A {:Id 1 :X 10}
           :I980/R1
           [{:I980/B {:Id 2 :Y 20}
             :I980/R2 {:I980/C {:Id 3 :Z 30}}}
            {:I980/B {:Id 4 :Y 40}
             :I980/R2 {:C 3}}]}))
  (is (cn/instance-of? :I980/A (tu/result {:I980/Temp {}})))
  (is (= :I980/Temp (cn/remove-event :I980/Temp)))
  (is (nil? (seq (tu/eval-all-dataflows {:I980/Temp {}})))))

(deftest issue-1051-between-node-names
  (tu/reset-events!)
  (defcomponent :I1051
    (entity
     :I1051/A
     {:Id {:type :Int :identity true}
      :X :Int
      :rbac [{:roles ["i1051-user"] :allow [:create]}]})
    (entity
     :I1051/B
     {:Id {:type :Int :identity true}
      :Y :Int
      :rbac [{:roles ["i1051-user"] :allow [:create]}]})
    (relationship
     :I1051/R1
     {:meta {:between [:I1051/A :I1051/B]}
      :rbac {:owner :I1051/A}})
    (relationship
     :I1051/R2
     {:meta {:between [:I1051/A :I1051/A :as [:I :J]]}
      :rbac {:owner :J}})
    (dataflow
     :I1051/InitUsers
     {:Fractl.Kernel.Identity/User
      {:Email "u1@i1051.com"}}
     {:Fractl.Kernel.Identity/User
      {:Email "u2@i1051.com"}}
     {:Fractl.Kernel.Rbac/RoleAssignment
      {:Role "i1051-user" :Assignee "u2@i1051.com"}}
     {:Fractl.Kernel.Rbac/RoleAssignment
      {:Role "i1051-user" :Assignee "u1@i1051.com"}}))
  (is (tu/finalize-events))
  (is (cn/instance-of?
       :Fractl.Kernel.Rbac/RoleAssignment
       (tu/first-result {:I1051/InitUsers {}})))
  (tu/call-with-rbac
   (fn []
     (let [create-a (fn [with-user id]
                      (tu/first-result
                       (with-user
                         {:I1051/Create_A
                          {:Instance
                           {:I1051/A {:Id id :X (* id 10)}}}})))
           a? (partial cn/instance-of? :I1051/A)
           wu1 (partial tu/with-user "u1@i1051.com")
           wu2 (partial tu/with-user "u2@i1051.com")
           a1 (create-a wu1 1), a2 (create-a wu2 2)]
       (is (a? a1)) (is (a? a2))))))
