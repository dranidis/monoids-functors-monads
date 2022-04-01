(ns monoids-functors-monads.action)

;; wrapping actions into functions
(defn return [v]
  (fn [] v))

(defn bind [a f]
  (let [v (a)]
    (println "v:" v)
    (if (nil? v) (return nil)
        (f v))))

;; (defn f-inc [v]
;;   (return (inc v)))

;; ((-> (return 5)
;;      (bind f-inc)
;;      (bind f-inc)))

(defn m-inc [a]
  (bind a (fn [v] (return (inc v)))))

((-> (return 5)
     (m-inc)
     (m-inc)))

(defn m-add [a n]
  (bind a (fn [v] (return (+ v n)))))

(defn m-div [a n]
  (if (zero? n)
    (return nil)
    (bind a (fn [v] (return (/ v n))))))

((-> (return 5)
     (m-add 15)
     (m-div 2)
     m-inc
     m-inc))

((-> (return 5)
     (m-add 6)
     (m-div 0)
     m-inc
     m-inc))