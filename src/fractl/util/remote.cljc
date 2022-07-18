(ns fractl.util.remote
  (:require [fractl.util :as u]
            [fractl.util.http :as uh]
            [fractl.util.auth :as au]
            [fractl.lang.internal :as li]
            [fractl.component :as cn]))

(defn- event-name [event-type inst]
  (let [[component inst-name] (li/split-path (cn/instance-type inst))]
    (str (name component) "/" (name event-type) "_" (name inst-name))))

(defn- remote-request [event-type mkobj host options inst]
  (let [en (event-name event-type inst)
        url (str host uh/entity-event-prefix en)
        request-obj {en (if mkobj
                          (mkobj inst)
                          {:Instance inst})}]
    (uh/POST url options request-obj)))

(defn- mk-lookup-obj [inst]
  {cn/id-attr (cn/id-attr inst)})

(def remote-upsert (partial remote-request :Upsert nil))
(def remote-get (partial remote-request :Lookup mk-lookup-obj))

(defn remote-delete [host options arg]
  (remote-request
   :Delete nil host options
   (if (map? arg)
     [(cn/instance-type arg) (cn/id-attr arg)]
     arg)))

(defn remote-query [host options query]
  (let [response (uh/POST
                  (str host uh/query-prefix)
                  options {:Query query})]
    (if (map? response)
      (first (:body response))
      response)))

(defn remote-eval
  ([host res arg event-inst]
   (uh/POST (str host res)
            (uh/normalize-post-options arg) event-inst))
  ([host arg event-inst]
   (remote-eval uh/dynamic-eval-prefix arg event-inst)))

(defn remote-login
  ([host callback username password]
   (remote-eval
    host uh/login-prefix callback
    (au/make-login-event username password)))
  ([host callback event-inst]
   (if (au/login-event-instance? event-inst)
     (remote-eval host uh/login-prefix callback event-inst)
     (if-let [ord (cn/display-order event-inst)]
       (remote-login host callback ((first ord) event-inst)
                     ((second ord) event-inst))
       (u/throw-ex
        (str "cannot derive login event from "
             (cn/instance-type event-inst)))))))
