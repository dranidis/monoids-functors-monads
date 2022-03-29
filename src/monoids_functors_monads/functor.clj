(ns monoids-functors-monads.functor)

(defprotocol Functor
  (fmap [j f]))