(ns fractl.user-session
  (:require [fractl.util :as u]
            [fractl.evaluator :as ev]))

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

(defn session-create [user logged-in user-data]
  (ev/eval-internal
   {:Fractl.Kernel.Identity/Create_UserSession
    {:Instance
     {:Fractl.Kernel.Identity/UserSession
      {:User user :LoggedIn logged-in :UserData user-data}}}}))

(defn session-update [user logged-in user-data]
  (ev/eval-internal
   {:Fractl.Kernel.Identity/Update_UserSession
    {:User user :Data {:LoggedIn logged-in :UserData user-data}}}))

(defn session-cookie-create [sid id-token]
  (ev/eval-internal
   {:Fractl.Kernel.Identity/Create_SessionCookie
    {:Instance
     {:Fractl.Kernel.Identity/SessionCookie
      {:Id sid :Token id-token}}}}))

(defn lookup-token-from-session-cookie [sid]
  (let [result (ev/eval-internal
                {:Fractl.Kernel.Identity/Lookup_SessionCookie
                 {:Id sid}})
        r (if (map? result) result (first result))
        s (:status r)
        obj (when (= s :ok) (first (:result r)))]
    (when obj (:Token obj))))
