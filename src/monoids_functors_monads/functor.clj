(ns monoids-functors-monads.functor)

(defprotocol Functor
  (fmap [j f]))

(defprotocol Applicative
  (fapply [f v]))

(defprotocol Monad
  (bind [m f]))

(defrecord Maybe [value]
  Functor
  Applicative
  Monad
  ;; (M (a->b)) (M a) -> (M b)  
  (fapply [_ maybe-val]
    (if (or (nil? value) (nil? (:value maybe-val)))
      (Maybe. nil)
      (Maybe. (value (:value maybe-val)))))
  ;; (M a) (a -> b) -> (M b)
  (fmap [_ fun]
        ;; (-> (Maybe. fun) (fapply value))
    (if (nil? value)
      (Maybe. nil)
      (Maybe. (fun value))))

  ;; unwraps a monadic variable, 
  ;; then inserts it into a monadic function/expression, 
  ;; resulting in a new monadic value:
  ;; (M a) (a -> (M b)) -> (M b)
  (bind [_ fun]
    (if (nil? value)
      (Maybe. nil)
      (fun value))))

(comment

  (-> (Maybe. inc)
      (fapply (Maybe. 1)))

  (-> (Maybe. inc)
      (fapply (Maybe. nil)))

  (-> (Maybe. nil)
      (fapply (Maybe. 1)))

  (-> (Maybe. 0)
      (fmap inc))

  (-> (Maybe. nil)
      (fmap inc))


  (-> (Maybe. (fn [a] (fn [b] (+ a b))))
      (fapply (Maybe. 1))
      (fapply (Maybe. 2)))

  (-> (Maybe. (fn [a] (fn [b] (+ a b))))
      (fapply (Maybe. 1))
      (fapply (Maybe. 2)))

  (-> (Maybe. (fn [a] (fn [b] (+ a b))))
      (fapply (Maybe. nil))
      (fapply (Maybe. 2)))


  (-> (Maybe. (fn [a] (fn [b] (+ a b))))
      (fapply (Maybe. 1))
      (fapply (Maybe. nil)))

  (defn half [x]
    (if (even? x)
      (Maybe. (quot x 2))
      (Maybe. nil)))

  (-> (Maybe. 20)
      (bind half)
      (bind half)
      (bind half)
      (bind half))

  (defn add [a b]
    (if (or (nil? b) (nil? a))
      (Maybe. nil)
      (Maybe. (+ a b))))

  (-> (Maybe. 0)
      (bind #(add % 10))
      (bind half)
      (bind #(add % 20)))


  (defn div [a b] (if (= 0 b) (Maybe. nil) (Maybe. (/ a b))))

  (defn chainable-division [mx my]
    (-> mx (bind (fn [x]
                   (-> my (bind (fn [y]
                                  (div x y))))))))

  (-> (chainable-division (Maybe. 2) (Maybe. 4))
      (chainable-division (Maybe. 0))
      (chainable-division (Maybe. 3)))
  ;
  )

;; identity
(= (-> (Maybe. identity) (fapply (Maybe. 1)))
   (Maybe. 1))

;; composition ???

;; homomorphism
(= (-> (Maybe. inc) (fapply (Maybe. 0)))
   (Maybe. (inc 0)))

;; interchange ??


;; Monad laws
;;
;; left-identity
(defn f [x] (Maybe. x))
(= (-> (Maybe. 1) (bind f))
   (f 1))
;; right-identity
(= (-> (Maybe. 1) (bind (fn [x] (Maybe. x))))
   (Maybe. 1))