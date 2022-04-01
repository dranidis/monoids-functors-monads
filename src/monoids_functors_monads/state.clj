(ns monoids-functors-monads.state
  (:require [clojure.spec.alpha :as s]))

(defprotocol Monad
  (bind [m f]))

(defprotocol Applicative
  (fapply [fm v]))

(s/def ::state-monad
  (s/keys :req-un [::state-transfomer]))

(defrecord State [state-transfomer]
  Monad
  Applicative
  (bind
    [state fun]
    {:pre [(s/valid? ::state-monad state)]}
    (->State (fn [state]
               (let [[x state'] (state-transfomer state)]
                ;;  (println {:value x :state state'})
                 (fapply (fun x) state')))))
  (fapply
    [st state-val]
    {:pre [(s/valid? ::state-monad st)]}
    (if state-transfomer
      (state-transfomer state-val)
      (let [_ (println "No state transformer: " st state-val)]
        nil))))

;; constructors for State
(defn return [v] (->State (fn [s] [v s])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-state [v]
  (->State (fn [_]
             [v v])))

(defn reset-state [v]
  (->State (fn [_]
             [v 0])))


(fapply
 (-> (return 5)
    ;;  ) 0)
     (bind set-state)
     (bind reset-state))
 0)









