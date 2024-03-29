(ns fractl.user-session
  (:require [clojure.string :as s]
            [fractl.util :as u]
            [fractl.evaluator :as ev]
            [fractl.util.logger :as log]))

(defn session-lookup [user]
  (let [result (ev/eval-internal
                {:Fractl.Kernel.Identity/Lookup_UserSession
                 {:User user}})
        r (if (map? result) result (first result))
        s (:status r)
        obj (if (= s :ok) (first (:result r)) (:result r))]
    [s obj]))

(defn is-logged-in [user]
  (let [[status session] (session-lookup user)]
    (if (= :ok status)
      (:LoggedIn session)
      (u/throw-ex (str "failed to lookup session for user " user)))))

(defn not-found? [session]
  (= :not-found (first session)))

(defn session-exists-for? [user]
  (not (not-found? (session-lookup user))))

(defn session-create [user logged-in]
  (ev/eval-internal
   {:Fractl.Kernel.Identity/Create_UserSession
    {:Instance
     {:Fractl.Kernel.Identity/UserSession
      {:User user :LoggedIn logged-in}}}}))

(defn session-update [user logged-in]
  (ev/eval-internal
   {:Fractl.Kernel.Identity/Update_UserSession
    {:User user :Data {:LoggedIn logged-in}}}))

(defn upsert-user-session [user-id logged-in]
  ((if (session-exists-for? user-id)
     session-update
     session-create)
   user-id logged-in))

(defn- normalize-sid [sid]
  (if-let [i (s/index-of sid "=")]
    (subs sid (inc i))
    sid))

(defn session-cookie-create [sid user-data]
  (ev/eval-internal
   {:Fractl.Kernel.Identity/Create_SessionCookie
    {:Instance
     {:Fractl.Kernel.Identity/SessionCookie
      {:Id (normalize-sid sid) :UserData user-data}}}}))

(defn session-cookie-delete [sid]
  (first
   (:result
    (first
     (ev/eval-internal
      {:Fractl.Kernel.Identity/Delete_SessionCookie
       {:Id (normalize-sid sid)}})))))

(defn session-cookie-replace [sid user-data]
  (session-cookie-delete sid)
  (session-cookie-create sid user-data))

(defn lookup-session-cookie-user-data [sid]
  (let [result (ev/eval-internal
                {:Fractl.Kernel.Identity/Lookup_SessionCookie
                 {:Id (normalize-sid sid)}})
        r (if (map? result) result (first result))
        s (:status r)]
    (when (= s :ok) (:UserData (first (:result r))))))

(defn session-cookie-update-tokens [sid tokens]
  (when-let [user-data (lookup-session-cookie-user-data sid)]
    (let [authr (merge (:authentication-result user-data) tokens)
          user-data (assoc user-data :authentication-result authr)]
      (ev/eval-internal
       {:Fractl.Kernel.Identity/Update_SessionCookie
        {:Id (normalize-sid sid) :Data {:UserData user-data}}})
      user-data)))
