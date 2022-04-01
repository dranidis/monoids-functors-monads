(ns monoids-functors-monads.action-state)

;; wrapping actions into functions
;; a -> (s -> (a s))
(defn return [v]
  ;; pass a state to the action
  ;; returns a value and a state
  (fn [s] [v s]))

;; (s -> (a s))  (a -> (s -> (a s)))  (s -> (a s))
(defn bind [a f]
  (fn [s]
    (let [[v s'] (a s)]
      (println "[v s]" [v s'])
      ((f v) s'))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; same definitions as in action from now on

(defn m-inc [a]
  (bind a (fn [v] (return (inc v)))))

;; ((-> (return 5)
;;      (m-inc)
;;      (m-inc)))

(defn m-add [a n]
  (bind a (fn [v] (return (+ v n)))))

(defn m-div [a n]
  (if (zero? n)
    (return nil)
    (bind a (fn [v] (return (/ v n))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pass a state when calling

((-> (return 5)
     (m-add 6)
     (m-div 2)
     m-inc
     m-inc) 0)

;; ((-> (return 5)
;;      (m-add 6)
;;      (m-div 0)
;;      m-inc
;;      m-inc) 0)



;;;;; 
(defn set-value [v]
  (fn [s] [v v]))

(defn set-to-state [a]
  (bind a set-value))

(def get-state (fn [s] [s s]))

;; (defn add-from-state [a]
;;   (bind a (fn [v]
;;             (fn [s] [(+ v s) s]))))

(defn add-from-state [a]
  (bind a (fn [v]
            (bind get-state (fn [s]
                              (return (+ s v)))))))

((-> (return 5)
     (m-add 5)
     set-to-state
     m-inc
     add-from-state
     m-inc) 0)
