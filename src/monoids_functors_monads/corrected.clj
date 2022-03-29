(ns monoids-functors-monads.corrected 
  (:require [monoids-functors-monads.functor :refer [Functor]]))

(deftype Nothing
  []
  Functor
  (fmap [_ _] (Nothing.)))

(deftype Maybe [value]
  Functor
  (fmap [_ f] (if (nil? value)
                (Nothing.)
                (Maybe. (f value)))))

