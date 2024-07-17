(ns fractl.test.graphql
  (:require [clojure.test :refer :all]
            [fractl.util.errors :refer :all]
            [fractl.test.util :as test-util]
            [clojure.spec.alpha :as s]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [ring.util.response :refer [response]]
            [clojure.test :refer [deftest is testing]]
            [fractl.test.util :as tu :refer [defcomponent]]
            [fractl.api :as api]
            [fractl.evaluator :as e]
            [ring.util.response :refer [response]]
            [com.walmartlabs.lacinia :refer [execute]]
            [fractl.component :as cn]
            [fractl.graphql.generator :as gg]
            [fractl.graphql.core :as graphql]
            [fractl.lang
             :as ln
             :refer [component attribute event entity record dataflow relationship]]
            [fractl.lang.internal :as li]
            [clojure.walk :as walk]
            [fractl.graphql.resolvers :as gr])
  (:import (clojure.lang IPersistentMap)))

(defn simplify
  "Converts all ordered maps nested within the map into standard hash maps, and
   sequences into vectors, which makes for easier constants in the tests, and eliminates ordering problems."
  [m]
  (walk/postwalk
    (fn [node]
      (cond
        (instance? IPersistentMap node)
        (into {} node)

        (seq? node)
        (vec node)

        :else
        node))
    m))

(defn graphql-handler
  ([component-name query variables]
   (let [schema (cn/schema-info component-name)
        contains-graph-map (gg/generate-contains-graph schema)
        [uninjected-graphql-schema injected-graphql-schema entity-metadatas] (graphql/compile-graphql-schema schema contains-graph-map)]
    (let [context {:auth-config nil :core-component component-name :contains-graph contains-graph-map :entity-metas entity-metadatas}
          result (simplify (execute injected-graphql-schema query variables context))]
        (:data result))))
  ([component-name query]
   (graphql-handler component-name query nil)))

(defn filter-event-attrs [event]
  "Removes internal attrs from event."
    (dissoc (fractl.component/instance-user-attributes event) :EventContext :__path__ :__parent__))

(defn build-sample-app []
  (cn/remove-component :GraphQL.Test)
  (api/component :GraphQL.Test)
  (api/entity
    :GraphQL.Test/F
    {:Id :Identity
     :Name {:type :String :id true}
     :Y :Int})
  (tu/finalize-component :GraphQL.Test))

(deftest test-create-user-mutation
  (build-sample-app)
  (let [query (str "mutation {
                    CreateF(
                        input: {
                            Name: \"hasnain\"
                            Id: \"0e977860-5cd4-4bc3-8323-f4f71a66de6d\"
                            Y: 1
                        }
                    ) {
                        Name
                        Id
                        Y
                    }
                }")
        expected {:Name "hasnain"
                  :Id "0e977860-5cd4-4bc3-8323-f4f71a66de6d"
                  :Y 1}
        results (graphql-handler :GraphQL.Test query)]
   (is (= expected (:CreateF results)))))

(defn build-word-count-app []
  (cn/remove-component :WordCount.Core)
  (defcomponent :WordCount.Core
    (entity :WordCount.Core/User
            {:Id :Identity
             :Email {:type :Email}
             :Name :String
             :MemberSince {:type :Date :optional true}})

    (entity :WordCount.Core/Hero
           {:Id {:type :Int :guid true}
            :Name :String
            :HomePlanet :String
            :Age :Int
            :ForceSensitive :Boolean})

    (entity :WordCount.Core/Order
        {:Id        :Int
         :Details   :String
         :CreatedAt :DateTime})

    (entity :WordCount.Core/Profile
            {:Id :Identity
             :Name :String
             :Bio :String})

    (entity :WordCount.Core/Document
            {:Id :Identity
             :Name {:type :String :id true}
             :Content :String
             :Summary :String
             :LastUpdated {:type :DateTime :optional true}})

    (entity :WordCount.Core/Tag
            {:Id :Identity
             :Name :String})

    (entity :WordCount.Core/Page
            {:Id :Identity
             :Name :String})

    (entity :WordCount.Core/Index
            {:Id :Identity
             :Name :String})

    ;; RELATIONSHIPS

    (relationship :WordCount.Core/UserDocument
                  {:meta {:contains [:WordCount.Core/User :WordCount.Core/Document]}})

    (relationship :WordCount.Core/UserProfile
                  {:meta {:contains [:WordCount.Core/User :WordCount.Core/Profile]}})

    (relationship :WordCount.Core/DocumentPage
                  {:meta {:contains [:WordCount.Core/Document :WordCount.Core/Page]}})

    (relationship :WordCount.Core/DocumentIndex
                  {:meta {:contains [:WordCount.Core/Document :WordCount.Core/Index]}})

    (relationship :WordCount.Core/UserTag
                  {:meta {:between [:WordCount.Core/User :WordCount.Core/Tag]}
                   :Awesome {:type :String}})

    (relationship :WordCount.Core/DocumentTag
                  {:meta {:between [:WordCount.Core/Document :WordCount.Core/Tag]}
                   :MetaDetails :String})

    (relationship :WordCount.Core/DocumentDocuments
                  {:meta {:between [:WordCount.Core/Document :WordCount.Core/Document :as [:Document1 :Document2]]}
                   :Details {:type :String :optional true}})

    (dataflow
     :WordCount.Core/LookupTags
     {:WordCount.Core/DocumentTag
      {:Document? :WordCount.Core/LookupTags.Document
       :Tag? :WordCount.Core/LookupTags.Tag}})))

(deftest test-queries-for-word-count-app
  (build-word-count-app)
  (let [document-data {:Id "0e977860-5cd4-4bc3-8323-f4f71a66de6e"
                       :Name "Sample Document"
                       :Content "This is a sample document content."
                       :Summary "Summary of the document."}

        user-data {:Id "0e977860-5cd4-4bc3-8323-f4f71a66de6d"
                   :Email "user17@example.com"
                   :Name "John Doe"}

        heroes [{:Id 1 :Name "Luke Skywalker" :HomePlanet "Tatooine" :Age 23 :ForceSensitive true}
               {:Id 2 :Name "Leia Organa" :HomePlanet "Alderaan" :Age 23 :ForceSensitive true}
               {:Id 3 :Name "Han Solo" :HomePlanet "Corellia" :Age 32 :ForceSensitive false}
               {:Id 4 :Name "Chewbacca" :HomePlanet "Kashyyyk" :Age 200 :ForceSensitive false}]

        parent-user-data {:Email "user17@example.com"
                         :Name "John Doe"}

        tag-data {:Id "0e977860-5cd4-4bc3-8323-f4f71a66de6d"
                  :Name "Tag 1"}

        user-tag-data {:User (:Id user-data)
                       :Tag (:Id tag-data)
                       :Awesome "Nice"}

        query-by-id-pattern "query {
                               User(attributes: {Id: \"0e977860-5cd4-4bc3-8323-f4f71a66de6d\"}) {
                                   Id
                                   Email
                                   Name
                               }
                             }"

        query-by-email-pattern "query {
                                   User(attributes: {Email: \"user17@example.com\"}) {
                                       Email
                                       Name
                                   }
                                 }"

        query-by-name-pattern "query {
                                  User(attributes: {Name: \"John Doe\"}) {
                                      Email
                                      Name
                                  }
                                }"

        parent-user-email "user17@example.com"
        child-document-name "Sample Document"
        parent-user-id "0e977860-5cd4-4bc3-8323-f4f71a66de6d"
        child-document-id "0e977860-5cd4-4bc3-8323-f4f71a66de6e"

        query-all-docs-for-user-pattern (str "query {
                                                User(attributes: { Email: \"" parent-user-email "\" }) {
                                                    Email
                                                    UserDocument {
                                                        Document {
                                                            Name
                                                            Content
                                                            Summary
                                                            LastUpdated
                                                        }
                                                    }
                                                }
                                            }")

        query-by-email-and-doc-id-pattern (str "query {
                                                    User(attributes: { Email: \"" parent-user-email "\" }) {
                                                        Email
                                                        Name
                                                        UserDocument {
                                                            Document(attributes: { Id: \"" child-document-id "\" }) {
                                                                Name
                                                                Content
                                                                Summary
                                                                LastUpdated
                                                            }
                                                        }
                                                    }
                                                }")

        query-by-user-id-and-doc-name-pattern (str "query {
                                                  User(attributes: { Id: \"" parent-user-id "\" }) {
                                                      Name
                                                      UserDocument {
                                                          Document(attributes: { Name: \"" child-document-name "\" }) {
                                                              Name
                                                              Content
                                                              Summary
                                                              LastUpdated
                                                          }
                                                      }
                                                  }
                                              }")

        query-tag-by-all-attributes "query {
                                        UserTag(attributes: {
                                            User: \"0e977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                            Tag: \"0e977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                            Awesome: \"Nice\"
                                        }) {
                                            User
                                            Tag
                                            Awesome
                                        }
                                    }"

        query-tag-by-one-attribute "query {
                                        UserTag(attributes: {Awesome: \"Nice\"}) {
                                            User
                                            Tag
                                            Awesome
                                        }
                                    }"
        ]

    ;; CREATE AND QUERY PARENT
    (testing "Create instance of parent entity"
      (let [user-instance (first (tu/fresult
                                   (e/eval-all-dataflows
                                     (cn/make-instance
                                      :WordCount.Core/Create_User
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/User user-data)}))))]
        (is (cn/instance-of? :WordCount.Core/User user-instance))
        (is (= (filter-event-attrs user-instance) user-data))))

    (testing "Query Parent by GUID"
        (let [results (graphql-handler :WordCount.Core query-by-id-pattern)
              result-data (first (:User results))]
          (is (= user-data result-data))))

    (testing "Query Parent by ID"
      (let [results (graphql-handler :WordCount.Core query-by-email-pattern)
            result-data (first (:User results))]
        (is (= (dissoc user-data :Id) result-data))))

    (testing "Query Parent by Non-ID Attribute"
      (let [results (graphql-handler :WordCount.Core query-by-name-pattern)
            result-data  (first (:User results))]
        (is (= (dissoc user-data :Id) result-data))))

    (testing "Multi-Condition Filter Query"
      (let [multi-condition-query "query getMultiConditionFilteredHeros($filter: HeroFilter) {
                                      Hero (filter: $filter) {
                                        Id
                                        Name
                                        HomePlanet
                                        Age
                                        ForceSensitive
                                      }
                                    }"
            multi-condition-variables
            {:filter
             {:and [
                    {:or [
                          {:Age {:gte 30 :lt 100}}
                          {:and [
                                 {:Age {:gte 20 :lt 30}}
                                 {:ForceSensitive true}
                                 ]}
                          ]}
                    {:not {:HomePlanet {:eq "Kashyyyk"}}}
                    {:or [
                          {:Name {:contains "a"}}
                          {:HomePlanet {:in ["Tatooine" "Alderaan"]}}
                          ]}
                    ]}}]

      (mapv
        (fn [hero]
          (tu/fresult
            (e/eval-all-dataflows
              (cn/make-instance
                :WordCount.Core/Create_Hero
                {:Instance
                 (cn/make-instance :WordCount.Core/Hero hero)}))))
        heroes)
      (let [results (graphql-handler :WordCount.Core multi-condition-query multi-condition-variables)
            results (:Hero results)]
        (doseq [[result expected] (map vector results heroes)]
          (is (= result expected) (str "Mismatch for hero " (:Name expected)))))))

    (testing "Filtered Heroes Query"
      (let [filtered-heroes-query "query getFilteredHeroes($filter: HeroFilter, $limit: Int, $offset: Int) {
                                     Hero(filter: $filter, limit: $limit, offset: $offset) {
                                       Id Name HomePlanet Age ForceSensitive
                                     }
                                   }"
            filtered-heroes-variables {:filter {:and [{:Name {:startsWith "L"}}
                                                      {:HomePlanet {:contains "oo"}}
                                                      {:Age {:between [20 30]}}
                                                      {:ForceSensitive true}]}
                                       :limit  2
                                       :offset 0}
            results (graphql-handler :WordCount.Core filtered-heroes-query filtered-heroes-variables)
            results (:Hero results)]

        (is (= 1 (count results)) "Filtered heroes query returned wrong number of results")
          (is (= "Luke Skywalker" (:Name (first results))) "Filtered heroes query returned wrong hero")))

    (testing "Complex Filtered Heroes Query"
      (let [complex-filter-query "query getComplexFilteredHeros($filter: HeroFilter) {
              Hero(filter: $filter) {
                Id Name HomePlanet Age ForceSensitive
              }
            }"
            complex-filter-variables {:filter {:or [{:and [{:HomePlanet {:endsWith "ne"}}
                                                           {:ForceSensitive true}]}
                                                    {:and [{:Name {:contains "a"}}
                                                           {:Age {:gt 30}}]}
                                                    {:and [{:Age {:gte 100}}
                                                           {:ForceSensitive false}]}]}}]

        (let [results (graphql-handler :WordCount.Core complex-filter-query complex-filter-variables)
              results (:Hero results)]
          (is (= 3 (count results)) "Complex filtered heroes query returned wrong number of results")
          (is (some #(= "Chewbacca" (:Name %)) results) "Complex filtered heroes query should include Chewbacca")
          (is (some #(= "Luke Skywalker" (:Name %)) results) "Complex filtered heroes query should include Luke Skywalker"))))

    (testing "Deep Nested Filtered Heroes Query"
      (let [deep-nested-filter-query "query getDeepNestedFilteredHeros($filter: HeroFilter) {
              Hero(filter: $filter) {
                Id Name HomePlanet Age ForceSensitive
              }
            }"
            deep-nested-filter-variables {:filter
                                          {:and [
                                                 {:or [
                                                       {:and [
                                                              {:not {:Age {:lt 23}}}
                                                              {:HomePlanet {:eq "Tatooine"}}
                                                              ]}
                                                       {:and [
                                                              {:ForceSensitive true}
                                                              {:not {:or [
                                                                          {:Name {:contains "Solo"}}
                                                                          {:Name {:contains "Chewbacca"}}
                                                                          ]}}
                                                              ]}
                                                       ]}
                                                 {:or [
                                                       {:not {:and [
                                                                    {:Age {:gte 30}}
                                                                    {:Age {:lte 100}}
                                                                    ]}}
                                                       {:HomePlanet {:in ["Alderaan" "Tatooine"]}}
                                                       ]}
                                                 ]}}]

        (let [results (graphql-handler :WordCount.Core deep-nested-filter-query deep-nested-filter-variables)
              results (:Hero results)]
          (is (= 2 (count results)) "Deep nested filtered heroes query returned wrong number of results")
          (is (some #(= "Luke Skywalker" (:Name %)) results) "Deep nested filtered heroes query should include Luke Skywalker")
          (is (some #(= "Leia Organa" (:Name %)) results) "Deep nested filtered heroes query should include Leia Organa"))))

    (testing "Very Complex Filtered Heroes Query"
      (let [very-complex-filter-query "query getVeryComplexFilteredHeros($filter: HeroFilter) {
              Hero(filter: $filter) {
                Id Name HomePlanet Age ForceSensitive
              }
            }"
            very-complex-filter-variables
            {:filter
             {:or [
                   {:and [
                          {:not {:or [
                                      {:Age {:lt 20}}
                                      {:Age {:gt 50}}
                                      ]}}
                          {:HomePlanet {:in ["Tatooine" "Alderaan" "Corellia"]}}
                          {:or [
                                {:Name {:startsWith "L"}}
                                {:Name {:endsWith "lo"}}
                                {:not {:Name {:contains "Chew"}}}
                                ]}
                          ]}
                   {:not {:and [
                                {:ForceSensitive false}
                                {:or [
                                      {:Age {:between [30 50]}}
                                      {:not {:HomePlanet {:in ["Kashyyyk" "Tatooine"]}}}
                                      ]}
                                ]}}
                   {:and [
                          {:Age {:gt 100}}
                          {:not {:or [
                                      {:Name {:startsWith "L"}}
                                      {:Name {:endsWith "a"}}
                                      ]}}
                          {:or [
                                {:HomePlanet {:eq "Kashyyyk"}}
                                {:ForceSensitive false}
                                ]}
                          ]}
                   ]}}]

        (let [results (graphql-handler :WordCount.Core very-complex-filter-query very-complex-filter-variables)
              results (:Hero results)]
          (is (= 4 (count results)) "Very complex filtered heroes query returned wrong number of results")
          (is (some #(= "Luke Skywalker" (:Name %)) results) "Very complex filtered heroes query should include Luke Skywalker")
          (is (some #(= "Leia Organa" (:Name %)) results) "Very complex filtered heroes query should include Leia Organa")
          (is (some #(= "Han Solo" (:Name %)) results) "Very complex filtered heroes query should include Han Solo")
          (is (some #(= "Chewbacca" (:Name %)) results) "Very complex filtered heroes query should include Chewbacca"))))

    ;; CREATE AND QUERY CHILD
    (testing "Manually create instances of parent and child entities"
      (let [user-instance (first (tu/fresult
                                   (e/eval-all-dataflows
                                     (cn/make-instance
                                      :WordCount.Core/Create_User
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/User parent-user-data)}))))]
        (api/dataflow
         :WordCount.Core/CreateTestDocument
         {:WordCount.Core/User
          {:Id? (:Id user-instance)} :as :U}
          {:WordCount.Core/Document document-data
           :-> [[:WordCount.Core/UserDocument :U]]})

        (tu/finalize-component :WordCount.Core)
        (let [result (:result (first (e/eval-all-dataflows
                           (cn/make-instance {:WordCount.Core/CreateTestDocument {}}))))]
          (is (cn/instance-of? :WordCount.Core/Document result))
          (is (= (filter-event-attrs result) document-data)))))

    (testing "Query All Documents for User"
      (let [results (graphql-handler :WordCount.Core query-all-docs-for-user-pattern)]
        (is (not-empty (get-in results [:User 0 :UserDocument])))))

    (testing "Query Child by GUID Attribute"
        (let [results (graphql-handler :WordCount.Core query-by-email-and-doc-id-pattern)]
          (is (= child-document-name (get-in results [:User 0 :UserDocument 0 :Document 0 :Name])))))

    (testing "Query Child by Non-GUID Attribute"
      (let [results (graphql-handler :WordCount.Core query-by-user-id-and-doc-name-pattern)
            user-documents (get-in results [:User 0 :UserDocument])]
        (is (= "Sample Document" (get-in user-documents [0 :Document 0 :Name])))))

     ;; CREATE AND QUERY BETWEEN INSTANCE
    (testing "Create instance of between relationship"
      (let [user-instance (first (tu/fresult
                                   (e/eval-all-dataflows
                                     (cn/make-instance
                                      :WordCount.Core/Create_User
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/User parent-user-data)}))))
            tag-instance (first (tu/fresult
                                  (e/eval-all-dataflows
                                    (cn/make-instance
                                      :WordCount.Core/Create_Tag
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/Tag tag-data)}))))
            user-tag-instance (first (tu/fresult
                                  (e/eval-all-dataflows
                                    (cn/make-instance
                                      :WordCount.Core/Create_UserTag
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/UserTag user-tag-data)}))))]
        (is (cn/instance-of? :WordCount.Core/UserTag user-tag-instance))
        (is (= (filter-event-attrs user-tag-instance) user-tag-data))))

    (testing "Query between instance by all attributes"
      (let [results (graphql-handler :WordCount.Core query-tag-by-all-attributes)
            result-data (first (:UserTag results))]
        (is (= user-tag-data result-data))))

    (testing "Query between instance by one attribute"
      (let [results (graphql-handler :WordCount.Core query-tag-by-one-attribute)
            result-data (first (:UserTag results))]
        (is (= user-tag-data result-data))))

    (testing "Date and DateTime Comparison Operators"
      (let [base-date "2024-07-17T10:00"
            orders (vec (for [i (range 1 11)]
                          (let [date-time (-> (java.time.LocalDateTime/parse base-date)
                                              (.plusDays (dec i))
                                              (.truncatedTo java.time.temporal.ChronoUnit/MINUTES)
                                              .toString)
                                order (cn/make-instance
                                        :WordCount.Core/Order
                                        {:Id        i
                                         :Details   (str "Order #" i)
                                         :CreatedAt date-time})]
                            (first (tu/fresult
                                     (e/eval-all-dataflows
                                       (cn/make-instance
                                         :WordCount.Core/Create_Order
                                         {:Instance order})))))))
            query-template "query getOrders($dateFilter: String!) {
                              Order(filter: {
                                CreatedAt: {
                                  %s: $dateFilter
                                }
                              }) {
                                Id
                                Details
                                CreatedAt
                              }
                            }"]

        (testing "Less than or equal (lte) operator"
          (let [lte-query (format query-template "lte")
                lte-variables {:dateFilter "2024-07-21T10:00"}
                results (:Order (graphql-handler :WordCount.Core lte-query lte-variables))]
            (is (= 5 (count results)))
            (is (every? #(<= (:Id %) 5) results))))

        (testing "Less than (lt) operator"
          (let [lt-query (format query-template "lt")
                lt-variables {:dateFilter "2024-07-21T10:00"}
                results (:Order (graphql-handler :WordCount.Core lt-query lt-variables))]
            (is (= 4 (count results)))
            (is (every? #(< (:Id %) 5) results))))

        (testing "Greater than or equal (gte) operator"
          (let [gte-query (format query-template "gte")
                gte-variables {:dateFilter "2024-07-21T10:00"}
                results (:Order (graphql-handler :WordCount.Core gte-query gte-variables))]
            (is (= 6 (count results)))
            (is (every? #(>= (:Id %) 5) results))))

        (testing "Greater than (gt) operator"
          (let [gt-query (format query-template "gt")
                gt-variables {:dateFilter "2024-07-21T10:00"}
                results (:Order (graphql-handler :WordCount.Core gt-query gt-variables))]
            (is (= 5 (count results)))
            (is (every? #(> (:Id %) 5) results))))

        (testing "Edge case: Exact date match"
          (let [eq-query (format query-template "eq")
                eq-variables {:dateFilter "2024-07-21T10:00"}
                results (:Order (graphql-handler :WordCount.Core eq-query eq-variables))]
            (is (= 1 (count results)))
            (is (= 5 (:Id (first results))))))

        (testing "No results case"
          (let [gt-query (format query-template "gt")
                gt-variables {:dateFilter "2024-07-27T10:00"}
                results (:Order (graphql-handler :WordCount.Core gt-query gt-variables))]
            (is (empty? results))))

        (testing "All results case"
          (let [gte-query (format query-template "gte")
                gte-variables {:dateFilter "2024-07-17T10:00"}
                results (:Order (graphql-handler :WordCount.Core gte-query gte-variables))]
            (is (= 10 (count results)))
            (is (= (set (range 1 11)) (set (map :Id results))))))))))

(deftest test-create-mutations-for-word-count-app
  (build-word-count-app)
  (let [document-data {:Id "1e977860-5cd4-4bc3-8323-f4f71a66de6e"
                       :Name "1Sample Document"
                       :Content "1This is a sample document content."
                       :Summary "1Summary of the document."}

        user-data {:Id "1e977860-5cd4-4bc3-8323-f4f71a66de6d"
                   :Email "1user17@example.com"
                   :Name "1John Doe"}

        parent-user-data {:Email "1user17@example.com"
                         :Name "1John Doe"}

        tag-data {:Id "11977860-5cd4-4bc3-8323-f4f71a66de6d"
                  :Name "1Tag 1"}

        user-tag-data {:User (:Id user-data)
                       :Tag (:Id tag-data)
                       :Awesome "1Nice"}

        create-user-pattern "mutation {
                                CreateUser(input: {
                                    Id: \"1e977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                    Email: \"1user17@example.com\",
                                    Name: \"1John Doe\",
                                }) {
                                    Id
                                    Email
                                    Name
                                }
                            }"

        query-by-id-pattern "query {
                               User(attributes: {Id: \"1e977860-5cd4-4bc3-8323-f4f71a66de6d\"}) {
                                   Id
                                   Email
                                   Name
                               }
                             }"

        query-by-email-pattern "query {
                                   User(attributes: {Email: \"1user17@example.com\"}) {
                                       Email
                                       Name
                                   }
                                 }"

        query-by-name-pattern "query {
                                  User(attributes: {Name: \"1John Doe\"}) {
                                      Email
                                      Name
                                  }
                                }"

        parent-user-email "1user17@example.com"
        child-document-name "1Sample Document"
        parent-user-id "1e977860-5cd4-4bc3-8323-f4f71a66de6d"
        child-document-id "1e977860-5cd4-4bc3-8323-f4f71a66de6e"

        create-child-document-mutation "mutation {
                                          CreateUserDocument(input: {
                                            UserId: \"1e977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                            Id: \"1e977860-5cd4-4bc3-8323-f4f71a66de6e\",
                                            Name: \"1Sample Document\",
                                            Content: \"1This is a sample document content.\",
                                            Summary: \"1Summary of the document.\"
                                          }) {
                                            UserId
                                            Id
                                            Name
                                            Content
                                            Summary
                                          }
                                        }"

        query-all-docs-for-user-pattern (str "query {
                                                User(attributes: { Email: \"" parent-user-email "\" }) {
                                                    Email
                                                    UserDocument {
                                                        Document {
                                                            Name
                                                            Content
                                                            Summary
                                                            LastUpdated
                                                        }
                                                    }
                                                }
                                            }")

        query-by-email-and-doc-id-pattern (str "query {
                                                    User(attributes: { Email: \"" parent-user-email "\" }) {
                                                        Email
                                                        Name
                                                        UserDocument {
                                                            Document(attributes: { Id: \"" child-document-id "\" }) {
                                                                Name
                                                                Content
                                                                Summary
                                                                LastUpdated
                                                            }
                                                        }
                                                    }
                                                }")

        query-by-user-id-and-doc-name-pattern (str "query {
                                                  User(attributes: { Id: \"" parent-user-id "\" }) {
                                                      Name
                                                      UserDocument {
                                                          Document(attributes: { Name: \"" child-document-name "\" }) {
                                                              Name
                                                              Content
                                                              Summary
                                                              LastUpdated
                                                          }
                                                      }
                                                  }
                                              }")

        create-tag-mutation  "mutation {
                                CreateTag(input: {
                                  Id: \"11977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                  Name: \"1Tag 1\"
                                }) {
                                  Id
                                  Name
                                }
                              }"

        create-user-tag-mutation "mutation {
                                    CreateUserTag(input: {
                                      Tag: \"11977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                      User: \"1e977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                      Awesome: \"1Nice\"
                                    }) {
                                      Tag
                                      User
                                      Awesome
                                    }
                                  }"

        query-tag-by-all-attributes "query {
                                        UserTag(attributes: {
                                            User: \"1e977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                            Tag: \"11977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                            Awesome: \"1Nice\"
                                        }) {
                                            User
                                            Tag
                                            Awesome
                                        }
                                    }"

        query-tag-by-one-attribute "query {
                                        UserTag(attributes: {Awesome: \"1Nice\"}) {
                                            User
                                            Tag
                                            Awesome
                                        }
                                    }"
        ]

    ;; MUTATE AND QUERY PARENT
    (testing "Create instance of parent entity"
      (let [results (graphql-handler :WordCount.Core create-user-pattern)
            result-data (:CreateUser results)]
        (is (= user-data result-data))))

    (testing "Query Parent by GUID"
        (let [results (graphql-handler :WordCount.Core query-by-id-pattern)
              result-data (first (:User results))]
          (is (= user-data result-data))))

    (testing "Query Parent by ID"
      (let [results (graphql-handler :WordCount.Core query-by-email-pattern)
            result-data (first (:User results))]
        (is (= (dissoc user-data :Id) result-data))))

    (testing "Query Parent by Non-ID Attribute"
      (let [results (graphql-handler :WordCount.Core query-by-name-pattern)
            result-data  (first (:User results))]
        (is (= (dissoc user-data :Id) result-data))))

    ;; MUTATE AND QUERY CHILD
    (testing "Create Child Document Instance"
      (let [results (graphql-handler :WordCount.Core create-child-document-mutation)
            result-data (:CreateUserDocument results)]
        (is (= (assoc document-data :UserId (:Id user-data)) result-data))))

    (testing "Query All Documents for User"
      (let [results (graphql-handler :WordCount.Core query-all-docs-for-user-pattern)]
        (is (not-empty (get-in results [:User 0 :UserDocument])))))

    (testing "Query Child by GUID Attribute"
        (let [results (graphql-handler :WordCount.Core query-by-email-and-doc-id-pattern)]
          (is (= child-document-name (get-in results [:User 0 :UserDocument 0 :Document 0 :Name])))))

    (testing "Query Child by Non-GUID Attribute"
      (let [results (graphql-handler :WordCount.Core query-by-user-id-and-doc-name-pattern)
            user-documents (get-in results [:User 0 :UserDocument])]
        (is (= "1Sample Document" (get-in user-documents [0 :Document 0 :Name])))))

     ;; MUTATE AND QUERY BETWEEN INSTANCE
    (testing "Create instance of between relationship"
      ;; create tag
      (graphql-handler :WordCount.Core create-tag-mutation)
      ;; create between instance
      (let [results (graphql-handler :WordCount.Core create-user-tag-mutation)
            result-data (:CreateUserTag results)]
        (is (= user-tag-data result-data))))

    (testing "Query between instance by all attributes"
      (let [results (graphql-handler :WordCount.Core query-tag-by-all-attributes)
            result-data (first (:UserTag results))]
        (is (= user-tag-data result-data))))

    (testing "Query between instance by one attribute"
      (let [results (graphql-handler :WordCount.Core query-tag-by-one-attribute)
            result-data (first (:UserTag results))]
        (is (= user-tag-data result-data))))))

(deftest test-update-mutations-for-word-count-app
  (build-word-count-app)
  (let [document-data {:Id "2e977860-5cd4-4bc3-8323-f4f71a66de6e"
                       :Name "2SampleDocument"
                       :Content "2This is a sample document content."
                       :Summary "2Summary of the document."}

        updated-document-data {:Id "2e977860-5cd4-4bc3-8323-f4f71a66de6e"
                               :Name "2SampleDocument"
                               :Content "new2This is a sample document content."
                               :Summary "new2Summary of the document."}

        user-data {:Id "2e977860-5cd4-4bc3-8323-f4f71a66de6d"
                   :Email "2user17@example.com"
                   :Name "2John Doe"}

        updated-user-data {:Id "2e977860-5cd4-4bc3-8323-f4f71a66de6d"
                           :Email "newuser17@example.com"
                           :Name "newJohn Doe"}

        parent-user-data {:Id "2e977860-5cd4-4bc3-8323-f4f71a66d100"
                          :Email "2user17@example.com"
                          :Name "2John Doe"}

        tag-data {:Id "2e977860-5cd4-4bc3-8323-f4f71a66de6d"
                  :Name "2Tag 1"}

        user-tag-data {:User (:Id user-data)
                       :Tag (:Id tag-data)
                       :Awesome "2Nice"}

        updated-user-tag-data {:User (:Id user-data)
                               :Tag (:Id tag-data)
                               :Awesome "NewNice"}

        update-user-with-guid-pattern "mutation {
                                          UpdateUser(input: {
                                              Id: \"2e977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                              Email: \"newuser17@example.com\",
                                              Name: \"newJohn Doe\",
                                          }) {
                                              Id
                                              Email
                                              Name
                                          }
                                      }"

        update-child-document-using-id-mutation "mutation {
                                                  UpdateUserDocument(input: {
                                                    UserId: \"2e977860-5cd4-4bc3-8323-f4f71a66d100\",
                                                    Name: \"2SampleDocument\",
                                                    Content: \"new2This is a sample document content.\",
                                                    Summary: \"new2Summary of the document.\"
                                                  }) {
                                                    UserId
                                                    Id
                                                    Name
                                                    Content
                                                    Summary
                                                  }
                                                }"

        update-child-document-using-guid-mutation "mutation {
                                                    UpdateUserDocument(input: {
                                                      UserId: \"2e977860-5cd4-4bc3-8323-f4f71a66d100\",
                                                      Content: \"guid2This is a sample document content.\",
                                                      Summary: \"guid2Summary of the document.\"
                                                    }) {
                                                      UserId
                                                      Name
                                                      Content
                                                      Summary
                                                    }
                                                  }"

        update-child-document-using-no-user-id-mutation "mutation {
                                                          UpdateUserDocument(input: {
                                                            Name: \"noid2SampleDocument\",
                                                            Content: \"noid2This is a sample document content.\",
                                                            Summary: \"noid2Summary of the document.\"
                                                          }) {
                                                            UserId
                                                            Id
                                                            Name
                                                            Content
                                                            Summary
                                                          }
                                                        }"

        update-child-document-using-no-id-mutation "mutation {
                                                    UpdateUserDocument(input: {
                                                      UserId: \"2e977860-5cd4-4bc3-8323-f4f71a66d100\",\n
                                                      Content: \"noid2This is a sample document content.\",
                                                      Summary: \"noid2Summary of the document.\"
                                                    }) {
                                                      UserId
                                                      Id
                                                      Name
                                                      Content
                                                      Summary
                                                    }
                                                  }"

        update-user-tag-mutation "mutation {
                                    UpdateUserTag(input: {
                                      Tag: \"2e977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                      User: \"2e977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                      Awesome: \"NewNice\"
                                    }) {
                                      Tag
                                      User
                                      Awesome
                                    }
                                  }"

        update-user-tag-mutation-without-tag-id "mutation {
                                                  UpdateUserTag(input: {
                                                    User: \"2e977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                                    Awesome: \"NewNice\"
                                                  }) {
                                                    Tag
                                                    User
                                                    Awesome
                                                  }
                                                }"
        ]

    ;; CREATE AND UPATE PARENT
    (testing "Create instance of parent entity"
      (let [user-instance (first (tu/fresult
                                   (e/eval-all-dataflows
                                     (cn/make-instance
                                      :WordCount.Core/Create_User
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/User user-data)}))))]
        (is (cn/instance-of? :WordCount.Core/User user-instance))
        (is (= (filter-event-attrs user-instance) user-data))))

    (testing "Update Parent by GUID"
        (let [results (graphql-handler :WordCount.Core update-user-with-guid-pattern)
              result-data (:UpdateUser results)]
          (is (= updated-user-data result-data))))

    ;; CREATE AND UPDATE CHILD
    (testing "Manually create instances of parent and child entities"
      (let [user-instance (first (tu/fresult
                                   (e/eval-all-dataflows
                                     (cn/make-instance
                                      :WordCount.Core/Create_User
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/User parent-user-data)}))))]
        (api/dataflow
         :WordCount.Core/CreateTestDocument
         {:WordCount.Core/User
          {:Id? (:Id user-instance)} :as :U}
          {:WordCount.Core/Document document-data
           :-> [[:WordCount.Core/UserDocument :U]]})

        (tu/finalize-component :WordCount.Core)
        (let [result (:result (first (e/eval-all-dataflows
                           (cn/make-instance {:WordCount.Core/CreateTestDocument {}}))))]
          (is (cn/instance-of? :WordCount.Core/Document result))
          (is (= (filter-event-attrs result) document-data)))))

    (testing "Update Child Document of User using ID"
      (let [results (graphql-handler :WordCount.Core update-child-document-using-id-mutation)]
        (is (= updated-document-data (dissoc (:UpdateUserDocument results) :UserId)))))

    (testing "Fail to Update Child Document of User When No User ID"
      (let [result (try
                     (graphql-handler :WordCount.Core update-child-document-using-no-user-id-mutation)
                     (catch Exception e e))]
        (is (instance? Exception result))
        (is (.contains (.getMessage result) ":Id not provided for :WordCount.Core/User"))))

    (testing "Fail to Update Child Document of User using GUID"
      (let [result (try
                     (graphql-handler :WordCount.Core update-child-document-using-guid-mutation)
                     (catch Exception e e))]
        (is (instance? Exception result))
        (is (.contains (.getMessage result) "Name not provided for :WordCount.Core/Document"))))

    (testing "Fail to Update Child Document of User When No Document ID"
      (let [result (try
                     (graphql-handler :WordCount.Core update-child-document-using-no-id-mutation)
                     (catch Exception e e))]
        (is (instance? Exception result))
        (is (.contains (.getMessage result) "Name not provided for :WordCount.Core/Document"))))

    ;; CREATE AND UPDATE BETWEEN INSTANCE
    (testing "Create instance of between relationship"
      (let [user-instance (first (tu/fresult
                                   (e/eval-all-dataflows
                                     (cn/make-instance
                                      :WordCount.Core/Create_User
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/User parent-user-data)}))))
            tag-instance (first (tu/fresult
                                  (e/eval-all-dataflows
                                    (cn/make-instance
                                      :WordCount.Core/Create_Tag
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/Tag tag-data)}))))
            user-tag-instance (first (tu/fresult
                                  (e/eval-all-dataflows
                                    (cn/make-instance
                                      :WordCount.Core/Create_UserTag
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/UserTag user-tag-data)}))))]
        (is (cn/instance-of? :WordCount.Core/UserTag user-tag-instance))
        (is (= (filter-event-attrs user-tag-instance) user-tag-data))))

    (testing "Update Between Instance using GUIDs"
        (let [results (graphql-handler :WordCount.Core update-user-tag-mutation)
              result-data (:UpdateUserTag results)]
          (is (= updated-user-tag-data result-data))))

    (testing "Fail to Update Between Instance When Entity GUIDs Missing"
      (let [result (try
                     (graphql-handler :WordCount.Core update-user-tag-mutation-without-tag-id)
                     (catch Exception e e))]
        (is (instance? Exception result))
        (is (.contains (.getMessage result) "Error: GUID for ':WordCount.Core/Tag' not provided."))))))

(deftest test-delete-mutations-for-word-count-app
  (build-word-count-app)
  (let [document-data {:Id "3e977860-5cd4-4bc3-8323-f4f71a66de6e"
                       :Name "3Sample Document"
                       :Content "3This is a sample document content."
                       :Summary "3Summary of the document."}

        document-data2 {:Id "3e977860-5cd4-4bc3-8323-f4f71a66de6e"
                       :Name "3Sample Document"
                       :Content "3This is a sample document content."
                       :Summary "3Summary of the document."}

        user-data {:Id "3e977860-5cd4-4bc3-8323-f4f71a66de6d"
                   :Email "3user17@example.com"
                   :Name "3John Doe"}

        parent-user-data {:Id "32977860-5cd4-4bc3-8323-f4f71a66de6d"
                          :Email "3user17@example.com"
                          :Name "3John Doe"}

        tag-data {:Id "32977860-5cd9-4bc3-8323-f4f71a66de6d"
                  :Name "3Tag 1"}

        user-tag-data {:User (:Id parent-user-data)
                       :Tag (:Id tag-data)
                       :Awesome "3Nice"}

        delete-user-by-id "mutation {
                              DeleteUser(input: { Id: \"3e977860-5cd4-4bc3-8323-f4f71a66de6d\" }) {
                                Id
                                Email
                                Name
                              }
                            }"

        delete-child-document-using-id-mutation "mutation {
                                                  DeleteUserDocument(input: {
                                                    UserId: \"32977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                                    Id: \"3e977860-5cd4-4bc3-8323-f4f71a66de6e\",
                                                  }) {
                                                    Id
                                                    Name
                                                    Content
                                                    Summary
                                                  }
                                                }"

        delete-child-document-using-several-attrs-mutation "mutation {
                                                              DeleteUserDocument(input: {
                                                                UserId: \"32977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                                                Id: \"3e977860-5cd4-4bc3-8323-f4f71a66de6e\",
                                                                Name: \"3Sample Document\",
                                                                Content: \"3This is a sample document content.\",
                                                                Summary: \"3Summary of the document.\"
                                                              }) {
                                                                UserId
                                                                Id
                                                                Name
                                                                Content
                                                                Summary
                                                              }
                                                            }"

        delete-child-document-without-parent-id-mutation "mutation {
                                                            DeleteUserDocument(input: {
                                                              Id: \"3e977860-5cd4-4bc3-8323-f4f71a66de6e\",
                                                            }) {
                                                              Id
                                                              Name
                                                              Content
                                                              Summary
                                                            }
                                                          }"

        delete-user-tag-mutation "mutation {
                                    DeleteUserTag(input: {
                                      Tag: \"32977860-5cd9-4bc3-8323-f4f71a66de6d\",
                                      User: \"32977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                      Awesome: \"3Nice\"
                                    }) {
                                      Tag
                                      User
                                      Awesome
                                    }
                                  }"

        delete-user-tag-mutation-without-tag-id "mutation {
                                                  DeleteUserTag(input: {
                                                    User: \"32977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                                    Awesome: \"3Nice\"
                                                  }) {
                                                    Tag
                                                    User
                                                    Awesome
                                                  }
                                                }"
        ]

    ;; CREATE AND DELETE PARENT
    (testing "Create instance of parent entity"
      (let [user-instance (first (tu/fresult
                                   (e/eval-all-dataflows
                                     (cn/make-instance
                                      :WordCount.Core/Create_User
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/User user-data)}))))]
        (is (cn/instance-of? :WordCount.Core/User user-instance))
        (is (= (filter-event-attrs user-instance) user-data))))

    (testing "Delete Parent by GUID"
      (let [delete-results (first (:DeleteUser (graphql-handler :WordCount.Core delete-user-by-id)))]
        (is (= delete-results user-data))))

    ;; CREATE AND DELETE CHILD
    (testing "Manually create instance of child document"
      (let [user-instance (first (tu/fresult
                                   (e/eval-all-dataflows
                                     (cn/make-instance
                                      :WordCount.Core/Create_User
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/User parent-user-data)}))))]
        (api/dataflow
         :WordCount.Core/CreateTestDocument
         {:WordCount.Core/User
          {:Id? (:Id user-instance)} :as :U}
          {:WordCount.Core/Document document-data
           :-> [[:WordCount.Core/UserDocument :U]]})

        (tu/finalize-component :WordCount.Core)
        (let [result (:result (first (e/eval-all-dataflows
                                       (cn/make-instance {:WordCount.Core/CreateTestDocument {}}))))]
          (is (cn/instance-of? :WordCount.Core/Document result))
          (is (= (filter-event-attrs result) document-data)))))

    (testing "Delete Child by GUID"
      (let [delete-results (first (:DeleteUserDocument (graphql-handler :WordCount.Core delete-child-document-using-id-mutation)))]
        (is (= delete-results document-data))))

    (testing "Manually create instance of another child document"
      (api/dataflow
       :WordCount.Core/CreateTestDocument2
       {:WordCount.Core/User
        {:Id? (:Id parent-user-data)} :as :U}
        {:WordCount.Core/Document document-data2
         :-> [[:WordCount.Core/UserDocument :U]]})

      (tu/finalize-component :WordCount.Core)
      (let [result (:result (first (e/eval-all-dataflows
                                     (cn/make-instance {:WordCount.Core/CreateTestDocument2 {}}))))]
        (is (cn/instance-of? :WordCount.Core/Document result))
        (is (= (filter-event-attrs result) document-data))))

    (testing "Delete Child by Several Attributes"
      ;; delete
      (let [delete-results (first (:DeleteUserDocument (graphql-handler :WordCount.Core delete-child-document-using-several-attrs-mutation)))]
        (is (= delete-results (assoc document-data :UserId (:Id parent-user-data))))))

    (testing "Fail to Delete Child When Parent GUID Missing"
      (let [result (try
                     (graphql-handler :WordCount.Core delete-child-document-without-parent-id-mutation)
                     (catch Exception e e))]
        (is (instance? Exception result))
        (is (.contains (.getMessage result) "Error: UserId not provided for :WordCount.Core/User. It is needed to identify the parent entity."))))

     ;; CREATE AND DELETE BETWEEN INSTANCE
    (testing "Create instance of between relationship"
      (let [tag-instance (first (tu/fresult
                                  (e/eval-all-dataflows
                                    (cn/make-instance
                                      :WordCount.Core/Create_Tag
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/Tag tag-data)}))))
            user-tag-instance (first (tu/fresult
                                  (e/eval-all-dataflows
                                    (cn/make-instance
                                      :WordCount.Core/Create_UserTag
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/UserTag user-tag-data)}))))]
        (is (cn/instance-of? :WordCount.Core/UserTag user-tag-instance))
        (is (= (filter-event-attrs user-tag-instance) user-tag-data))))

    (testing "Delete Between Instance using GUIDs"
        (let [results (graphql-handler :WordCount.Core delete-user-tag-mutation)
              result-data (first (:DeleteUserTag results))]
          (is (= user-tag-data result-data))))

    (testing "Delete Between Instance using GUIDs"
      (e/eval-all-dataflows
        (cn/make-instance
          :WordCount.Core/Create_UserTag
          {:Instance
           (cn/make-instance :WordCount.Core/UserTag user-tag-data)}))
      (let [results (graphql-handler :WordCount.Core delete-user-tag-mutation-without-tag-id)
            result-data (first (:DeleteUserTag results))]
        (is (= user-tag-data result-data))))
     ))
