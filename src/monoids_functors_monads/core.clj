(ns monoids-functors-monads.core
  (:require [clojure.string :as s]
            [monoids-functors-monads.tree :as tree]))

;; https://marmelab.com/blog/2018/04/18/functional-programming-2-monoid.html




;; composing functions
((comp - /) 8 3)
((comp s/upper-case s/reverse) "hello")
(s/capitalize "fsdfds")


;; partial 
((partial + 10) 5)