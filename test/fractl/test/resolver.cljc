(ns fractl.test.resolver
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.lang
             :refer [component attribute event
                     relationship entity record dataflow]]
            [fractl.component :as cn]
            [fractl.store :as store]
            [fractl.evaluator :as e]
            [fractl.env :as env]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

#?(:clj
   (def store (store/open-default-store nil))
   :cljs
   (def store (store/open-default-store {:type :alasql})))

(defn- test-resolver [install-resolver resolver-name path]
  (let [f (fn [_ arg] arg)
        r (r/make-resolver
           resolver-name
           {:create {:handler identity
                     :xform {:in [f :EntityXformR01/EToEPrime]
                             :out [f :EntityXformR01/EPrimeToE]}}
            :delete {:handler identity
                     :xform {:in [f]}}}
           e/eval-pure-dataflows)]
    (install-resolver path r)))

(def compose-test-resolver (partial test-resolver rg/compose-resolver))
(def override-test-resolver (partial test-resolver rg/override-resolver))

(defn- persisted? [comp-name entity-instance]
  (let [id (cn/id-attr entity-instance)
        evt (cn/make-instance
             (keyword (str (name comp-name) "/Lookup_E"))
             {cn/id-attr id})
        result (e/eval-all-dataflows evt)
        r (first result)]
    (when-not (= :not-found (:status r))
      (let [e (first (:result r))]
        (cn/same-instance? entity-instance e)))))

(deftest r01
  (defcomponent :EntityXformR01
    (entity :EntityXformR01/EPrime
            {:X :Int})
    (event {:EntityXformR01/EToEPrime
            {:Instance :Entity}})
    (dataflow :EntityXformR01/EToEPrime
              {:EntityXformR01/EPrime
               {:X :EntityXformR01/EToEPrime.Instance.X
                cn/id-attr :EntityXformR01/EToEPrime.Instance.Id}})
    (event {:EntityXformR01/EPrimeToE
            {:Instance :Entity}}))
  (defcomponent :R01
    (entity {:R01/E {:X :Int}}))
  (dataflow :EntityXformR01/EPrimeToE
            {:R01/E {:X :EntityXformR01/EPrimeToE.Instance.X
                     cn/id-attr :EntityXformR01/EPrimeToE.Instance.Id}})
  (let [e (cn/make-instance :R01/E {:X 10})
        result (tu/fresult (e/eval-all-dataflows {:R01/Create_E {:Instance e}}))
        e01 (first result)]
    (is (cn/instance-of? :R01/E e01))
    (is (nil? (second result)))
    (is (persisted? :R01 e01)))
  (compose-test-resolver :TestResolver01 :R01/E)
  (let [e (cn/make-instance :R01/E {:X 10})
        result (tu/fresult (e/eval-all-dataflows {:R01/Create_E {:Instance e}}))
        e01 (first result)]
    (is (cn/instance-of? :R01/E e01))
    (is (persisted? :R01 e01))
    (let [id (cn/id-attr e01)
          result (first (tu/fresult (e/eval-all-dataflows {:R01/Delete_E {cn/id-attr id}})))]
      (is (= (cn/id-attr result) id)))))

(defn- test-resolver-r02 [install-resolver resolver-name path]
  (let [f (fn [_ arg] arg)
        r (r/make-resolver resolver-name {:create {:handler identity
                                                   :xform {:in [f :EntityXformR02/EToE]
                                                           :out [f :EntityXformR02/EToK]}}
                                          :delete {:handler identity
                                                   :xform {:in [f]}}}
                           e/eval-pure-dataflows)]
    (install-resolver path r)))

(def compose-test-resolver-r02 (partial test-resolver-r02 rg/compose-resolver))
(def override-test-resolver-r02 (partial test-resolver-r02 rg/override-resolver))

(deftest r02
  (defcomponent :EntityXformR02
    (entity :EntityXformR02/E
            {:X :Int})
    (event {:EntityXformR02/EToE
            {:Instance :Entity}})
    (dataflow :EntityXformR02/EToE
              {:EntityXformR02/E
               {:X :EntityXformR02/EToE.Instance.X
                cn/id-attr (tu/append-id :EntityXformR02/EToE.Instance)}})
    (event {:EntityXformR02/EToK
            {:Instance :Entity}}))
  (defcomponent :R02
    (entity {:R02/E {:X :Int}})
    (record {:R02/K {:X :Int cn/id-attr :UUID}}))
  (dataflow :EntityXformR02/EToK
            {:R02/K {:X :EntityXformR02/EToK.Instance.X
                     cn/id-attr (tu/append-id :EntityXformR02/EToK.Instance)}})
  (override-test-resolver-r02 :TestResolver02 :R02/E)
  (let [e (cn/make-instance :R02/E {:X 10})
        result (tu/fresult (e/eval-all-dataflows {:R02/Create_E {:Instance e}}))
        e01 (first result)]
    (is (cn/instance-of? :R02/K e01))
    (is (not (persisted? :R02 e01)))))

(defn- test-query-resolver [install-resolver resolver-name path]
  (let [r (r/make-resolver
           resolver-name
           {:query
            {:handler
             (fn [arg]
               (let [where (:where (second arg))
                     where-clause (if (and (vector? where)
                                           (= (first where) :and))
                                    (second where)
                                    where)
                     wild-card? (= where-clause :*)]
                 (if wild-card?
                   [(cn/make-instance :ResQueryAll/E {:X 1 :N "e01"})
                    (cn/make-instance :ResQueryAll/E {:X 2 :N "e02"})]
                   (when-let [id (nth where-clause 2)]
                     [(cn/make-instance :RQ/E {:X 1 cn/id-attr id})]))))}}
           #(e/eval-all-dataflows % store {}))]
    (install-resolver path r)))

(deftest query
  (defcomponent :RQ
    (entity {:RQ/E {:X :Int}}))
  (test-query-resolver rg/compose-resolver :RQResolver :RQ/E)
  (let [e (cn/make-instance :RQ/E {:X 10})
        e01 (first (tu/fresult (e/eval-all-dataflows {:RQ/Create_E {:Instance e}})))]
    (is (cn/instance-of? :RQ/E e01))
    (is (= 10 (:X e01)))
    (let [id (cn/id-attr e01)
          e02 (first (tu/fresult (e/eval-all-dataflows {:RQ/Lookup_E {cn/id-attr id}})))]
      (is (cn/instance-of? :RQ/E e02))
      ;(is (= id (cn/id-attr e02)))
      (is (= 1 (:X e02))))))

(deftest query-all
  (defcomponent :ResQueryAll
    (entity {:ResQueryAll/E {:X :Int :N :String}})
    (event {:ResQueryAll/AllE {}})
    (dataflow :ResQueryAll/AllE
              :ResQueryAll/E?))
  (test-query-resolver rg/compose-resolver :RQResolver :ResQueryAll/E)
  (let [es [(cn/make-instance :ResQueryAll/E {:X 1 :N "e01"})
            (cn/make-instance :ResQueryAll/E {:X 2 :N "e02"})]
        evts (map #(cn/make-instance :ResQueryAll/Create_E {:Instance %}) es)
        _ (doall (map tu/fresult (map #(e/eval-all-dataflows %) evts)))
        result (tu/fresult (e/eval-all-dataflows {:ResQueryAll/AllE {}}))]
    (doseq [r result]
      (is (cn/instance-of? :ResQueryAll/E r))
      (is (= (if (= 1 (:X r)) "e01" "e02") (:N r))))))

(defn- resolver-upsert [k inst]
  (assoc inst k 123))

(defn- make-resolver [n k]
  (r/make-resolver
   n {:create {:handler (partial resolver-upsert k)}}))

(deftest compose-test
  (defcomponent :CT
    (entity {:CT/E1 {:X :Int :N :String}})
    (entity {:CT/E2 {:X :Int :N :String}}))
  (rg/compose-resolver :CT/E1 (make-resolver :CTR1 :X))
  (rg/compose-resolver :CT/E2 (make-resolver :CTR2 :Y))
  (let [result1 (tu/fresult
                 (e/eval-all-dataflows
                  {:CT/Create_E1
                   {:Instance
                    {:CT/E1 {:X 100 :N "hello"}}}}))]
    (tu/is-error
     #(tu/fresult
       (e/eval-all-dataflows
        {:CT/Create_E2
         {:Instance
          {:CT/E2 {:X 200 :N "bye"}}}})))
    (is (cn/instance-of? :CT/E1 (first result1)))
    (is (= 123 (:X (first result1))))))

(def ^:private invoke-query-flag (atom true))

(defn- fetch-and-assert-id [env entity-name id]
  (let [store (env/get-store env)
        inst (store/lookup-by-id store entity-name id)]
    (is (= id (cn/id-attr inst)))
    nil))

(defn- invoke-query [env arg]
  (when @invoke-query-flag
    (fetch-and-assert-id env (first arg) (nth (:where (second arg)) 2))))

(defn- invoke-delete [env arg]
  (when-not (cn/an-instance? arg)
    (apply fetch-and-assert-id env arg))
  (reset! invoke-query-flag false)
  nil)

(defn- resolver-invoke [method env arg]
  (case method
    :query (invoke-query env arg)
    :delete (invoke-delete env arg)
    nil))

(defn- make-resolver-for-invoke [n]
  (r/make-resolver
   n {:invoke {:handler resolver-invoke}}))

(deftest invoke-test
  (defcomponent :IT
    (entity {:IT/E1 {:X :Int :N :String}}))
  (rg/compose-resolver :IT/E1 (make-resolver-for-invoke :ITR1))
  (let [r1 (first
            (tu/fresult
             (e/eval-all-dataflows
              {:IT/Create_E1
               {:Instance
                {:IT/E1 {:X 100 :N "hello"}}}})))
        id (cn/id-attr r1)
        r2 (first
            (tu/fresult
             (e/eval-all-dataflows
              {:IT/Lookup_E1
               {cn/id-attr id}})))
        r3 (first
            (tu/fresult
             (e/eval-all-dataflows
              {:IT/Delete_E1
               {cn/id-attr id}})))
        r4 (e/eval-all-dataflows
            {:IT/Lookup_E1
             {cn/id-attr id}})]
    (is (= id (cn/id-attr r2)))
    (is (= id (cn/id-attr r3)))
    (is (= :not-found (:status (first r4))))))

(defn- deployment-resolver [call-count]
  (let [r (r/make-resolver
           :fractl-deploy
           {:create
            {:handler
             (fn [inst]
               (swap! call-count inc)
               inst)}})]
    (rg/compose-resolver :FractlDeployment.Core/Deployment r)))

(deftest duplicate-eval-bug
  (defcomponent :FractlDeployment.Core
    (entity
     :FractlDeployment.Core/User
     {:Email {:type :String
              :indexed true
              :unique true}
      :UserSlug :String})

    (entity
     :FractlDeployment.Core/Deployment
     {:Id :Identity
      :Model :String
      :Org :String
      :Config {:type :Map :optional true}
      :ModelName {:type :String :optional true}
      :ModelShortName {:type :String :optional true}
      :Status {:type :String :default "Undeployed"}
      :DeploymentInfo {:type :Map :optional true}
      :CreatedDate :Now})

    (relationship
     :FractlDeployment.Core/UserDeployment
     {:meta {:contains [:FractlDeployment.Core/User :FractlDeployment.Core/Deployment
                        :cascade-on-delete true]}})

    (event
     :FractlDeployment.Core/CreateDeployment
     {:Model :String
      :Org :String
      :Config {:type :Map :optional true}})

    (dataflow
     :FractlDeployment.Core/CreateDeployment
     {:FractlDeployment.Core/User
      {:Email? :FractlDeployment.Core/CreateDeployment.EventContext.User}
      :as :U}
     {:FractlDeployment.Core/Deployment
      {:Model :FractlDeployment.Core/CreateDeployment.Model
       :Org :FractlDeployment.Core/CreateDeployment.Org
       :Config :FractlDeployment.Core/CreateDeployment.Config}
      :-> [[{:FractlDeployment.Core/UserDeployment {}} :U]]}))
  (let [call-count (atom 0)]
    (deployment-resolver call-count)
    (is (cn/instance-of?
         :FractlDeployment.Core/User
         (tu/first-result
          {:FractlDeployment.Core/Create_User
           {:Instance
            {:FractlDeployment.Core/User
             {:Email "deployer@fractl.io"
              :UserSlug "ok"}}}})))
    (is (cn/instance-of?
         :FractlDeployment.Core/Deployment
         (tu/first-result
          {:FractlDeployment.Core/CreateDeployment
           {:Model "blog"
            :Org "fractl"
            :Config {:service 8080}
            :EventContext {:User "deployer@fractl.io"}}})))
    (is (= 1 @call-count))))

(deftest issue-1091-delete
  (defcomponent :I1091
    (entity
     :I1091/E
     {:Id :Identity
      :X :Int}))
  (let [rdb (atom [])
        r (r/make-resolver
           :i1091
           {:create {:handler (fn [inst] (swap! rdb conj inst) inst)}
            :query {:handler (fn [[_ {where :where}]]
                               (let [[_ _ id] where]
                                 (filter #(= (:Id %) id) @rdb)))}
            :delete {:handler (fn [{id :Id :as inst}]
                                (reset! rdb (filter #(not= (:Id %) id) @rdb))
                                inst)}})]
    (rg/override-resolver :I1091/E r)
    (let [e1 (tu/first-result
              {:I1091/Create_E
               {:Instance
                {:I1091/E {:X 10}}}})
          e? (partial cn/instance-of? :I1091/E)]
      (is (e? e1))
      (is (cn/same-instance? e1 (tu/first-result {:I1091/Delete_E {:Id (:Id e1)}})))
      (is (nil? (seq @rdb))))))

(deftest future-eval
  (defcomponent :Feval
    (entity
     :Feval/E
     {:Id :Identity
      :X :Int})
    (entity :Feval/F {:E :UUID})
    (entity :Feval/Evt {:E :UUID})
    (dataflow
     :Feval/Evt
     {:Feval/E {:Id? :Feval/Evt.E} :as [:E]}
     :E))
  (let [fut (atom nil)
        r (r/make-resolver
           :Feval
           {:create {:handler (fn [inst]
                                (reset!
                                 fut
                                 (future (tu/eval-all-dataflows
                                          {:Feval/Evt {:E (:E inst)}})))
                                inst)}})]
    (rg/override-resolver :Feval/F r)
    (let [e1 (tu/first-result
              {:Feval/Create_E
               {:Instance
                {:Feval/E {:X 10}}}})
          e? (partial cn/instance-of? :Feval/E)
          f1 (tu/first-result
              {:Feval/Create_F
               {:Instance
                {:Feval/F {:E (:Id e1)}}}})
          f? (partial cn/instance-of? :Feval/F)]
      (is (e? e1))
      (is (f? f1))      
      (is (cn/same-instance? e1 (tu/fresult (deref @fut)))))))
