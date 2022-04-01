(ns monoids-functors-monads.tree
  (:require [monoids-functors-monads.state :as state :refer [bind fapply return ->State]]
            [clojure.spec.alpha :as s]))

;; tree represented as a hierarchical list
;; (def tree
;;   '(("a"
;;      "b")
;;     "c"))

(s/def ::tree (s/or
               :non-leaf (s/coll-of ::tree :count 2)
               :leaf #(not (coll? %))))

(defn tree-left [tree] (first tree))
(defn tree-right [tree] (second tree))
(defn is-leaf? [tree] (not (seq? tree)))


;; relabeling trees

;;
;; solution using atoms
;;
(def fresh-label (atom 0))
(defn tree-relabel-atom [tree]
  (if (is-leaf? tree)
    (let [fresh @fresh-label
          _ (swap! fresh-label inc)]
      fresh)
    (let [l (tree-left tree)
          r (tree-right tree)]
      [(tree-relabel-atom l)
       (tree-relabel-atom r)])))

(defn tree-relabel-s [tree]
  (reset! fresh-label 0)
  (tree-relabel-atom tree))


;;
;; solution using a number passed to each call
;;
(defn tree-relabel-n [tree n]
  (if (is-leaf? tree)
    {:t n :label (inc n)}
    (let [l (tree-relabel-n (tree-left tree) n)
          r (tree-relabel-n (tree-right tree) (:label l))]
      {:t  [(:t l) (:t r)] :label (:label r)})))

(defn tree-relabel-0
  [tree]
  (:t (tree-relabel-n tree 0)))


;;
;; solution using a state monad
;;
(def fresh (->State (fn [n] [n (inc n)])))

(defn tree-relabel-state
  [tree]
  {:pre [(s/valid? ::tree tree)]
   :post [(s/valid? ::state/state-monad %)]}
  (if (is-leaf? tree)
    (bind fresh (fn [n]
                  (return n)))
    (bind (tree-relabel-state (tree-left tree))
          (fn [l']
            (bind (tree-relabel-state (tree-right tree))
                  (fn [r']
                    (return [l' r'])))))))

(defn tree-relabel-1
  [tree]
  {:pre [(s/valid? ::tree tree)]
   :post [(s/valid? ::tree %)]}
  (first (fapply (tree-relabel-state tree) 0)))


;; execution

(def tree
  '(("a" "b") "c"))

(tree-relabel-s tree)
(tree-relabel-0 tree)
(tree-relabel-1 tree)


(tree-relabel-state tree)



