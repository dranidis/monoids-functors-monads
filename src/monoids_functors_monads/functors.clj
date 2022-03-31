(ns monoids-functors-monads.functors
  (:require [clojure.test :refer [is]]))

;; https://marmelab.com/blog/2018/09/26/functional-programming-3-functor-redone.html

;; https://fluokitten.uncomplicate.org/articles/functors_applicatives_monads_in_pictures.html

;; https://web.archive.org/web/20180218211616/http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html

;;;

(defprotocol Functor
  (fmap [j f]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Maybe [value]
  Functor
  (fmap [_ f] (if (nil? value)
                (Maybe. nil)
                (Maybe. (f value)))))

(def user {:name "Holmes" :address {:street "Baker Street" :number "221B"}})
(def homeless {:name "Holmes"})

(-> (Maybe. user)
    (fmap :address)
    (fmap :street)
    .value)

(-> (Maybe. homeless)
    (fmap :address)
    (fmap :street)
    .value)

(defmulti get-or-else (fn [m _] (class m)))
(defmethod get-or-else Maybe [maybe default-value]
  (if (nil? (.value maybe)) default-value (.value maybe)))

(-> (Maybe. homeless)
    (fmap :address)
    (fmap :street)
    (get-or-else "unknown address"))

(defn get-street
  [user]
  (-> (Maybe. user)
      (fmap :address)
      (fmap :street)
      (get-or-else "unknown address")))

(get-street user)
(get-street homeless)


;;;;;;;;;;

(defrecord Left [value]
  Functor
  (fmap [_ _] (Left. value)))

(defrecord Right [value]
  Functor
  (fmap [_ f] (Right. (f value))))

(defn validate-email
  [value]
  (if (re-matches #"\S+@\S+\.\S+" value)
    (Right. value)
    (Left. (Exception. "The given email is invalid"))))


(-> (validate-email "foo@example.com")
    .value)

(-> (validate-email "foo@example")
    .value)

(-> (validate-email "foo@example.com")
    (fmap #(str "Email: " %))
    .value)

(-> (validate-email "foo@example")
    (fmap #(str "Email: " %))
    .value)

(defmulti catch (fn [m _] (class m)))
(defmethod catch Right [m _] (Right. (.value m)))
(defmethod catch Left [m f] (Right. (f (.value m))))

(-> (Right. "v")
    (catch #(%))
    .value)

(-> (Left. (Exception. "Invalid mail"))
    (catch (fn [e] (.getMessage e)))
    .value)

(defn try-catch
  [fun]
  (fn [value]
    (try (Right. (fun value))
         (catch Exception e (Left. e)))))

(def validate-email-try
  (try-catch (fn [value]
               (if (re-matches #"\S+@\S+\.\S+" value)
                 value
                 (throw (Exception. "The given email is invalid"))))))

(-> (validate-email-try "foo@example.com")
    (fmap #(str "Email: " %))
    (catch (fn [e] (.getMessage e)))
    .value)

(-> (validate-email-try "foo@example")
    (fmap #(str "Email: " %))
    (catch (fn [e] (.getMessage e)))
    .value)

(defn validate-user
  [user]
  (-> (Maybe. user)
      (fmap :email)
      (fmap (fn [v] (-> (validate-email-try v)
                        (catch (fn [e] (.getMessage e))))))))

(-> (validate-user {:first-name "John"
                    :email "foo@example.com"})
    .value
    .value)

(.value (.value (validate-user {:first-name "John"
                                :email "foo@example"})))

(.value (validate-user {:first-name "John"}))

(defn validate-user-value
  [user]
  (let [result (.value (validate-user user))]
    (if (nil? result) nil (.value result))))

(validate-user-value {:first-name "John"
                      :email "foo@example.com"})

(validate-user-value {:first-name "John"
                      :email "foo@example"})

(validate-user-value {:first-name "John"})

;;;;;;;;;;;;;;;;;;;;;;

(defmulti flattenval (fn [m] (class m)))
(defmethod flattenval Maybe [m] (if (nil? (.value m)) (Maybe. nil) (Maybe. (.value (.value m)))))

(.value (flattenval (Maybe. nil)))
(.value (flattenval (Maybe. (Maybe. 1))))
(.value (flattenval (Maybe. (Maybe. (Maybe. 1)))))

(defn validate-user-2
  [user]
  (-> (Maybe. user)
      (fmap :email)
      (fmap (fn [v] (-> (validate-email-try v)
                        (catch (fn [e] (.getMessage e))))))
      flattenval
      (get-or-else "The user has no email")))

(validate-user-2 {:first-name "John"
                  :email "foo@example.com"})

(validate-user-2 {:first-name "John"
                  :email "foo@example"})

(validate-user-2 {:first-name "John"})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti chain (fn [m _] (class m)))
(defmethod chain Maybe [m f] (-> m (fmap f) flattenval))

(defn validate-user-3
  [user]
  (-> (Maybe. user)
      (fmap :email)
      (chain (fn [v] (-> (validate-email-try v)
                         (catch (fn [e] (.getMessage e))))))
      (get-or-else "The user has no email")))

(validate-user-3 {:first-name "John"
                  :email "foo@example.com"})

(validate-user-3 {:first-name "John"
                  :email "foo@example"})

(validate-user-3 {:first-name "John"})



;; MONADS

;; A functor that can flatten itself with a chain method is called a Monad. 
;; So Maybe is a monad.

;; Left identity
;; f and g are any functions returning a Monad

(def x 1)
(defn f [v] (Maybe. v))
(defn g [v] (Maybe. (* 2 v)))

(is (=
     (-> (Maybe. x) (chain f) .value)
     (-> (f x) .value)))

;; Right identity
(is (=
     (-> (Maybe. x) (chain (fn [v] (Maybe. v))) .value)
     (-> (Maybe. x) .value)))

;; Associativity
(is (=
     (-> (Maybe. x) (chain f) (chain g) .value)
     (-> (Maybe. x) (chain (fn [v] (-> (f v) (chain g)))) .value)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> (Maybe. (Right. "bar@example.com"))
    flattenval
    .value)

(-> (Maybe. (Left. (Exception. "Invalid mail")))
    flattenval
    .value)

(defmethod flattenval Right [m] (.value m))

(defmethod chain Right [m f] (-> m (fmap f) flattenval))

(defn either->maybe
  [either]
  (-> either
      (catch (fn [e] nil))
      Maybe.
      flattenval))

(.value (either->maybe (Right. "bar@example.com")))
(.value (either->maybe (Left. (Exception. "Invalid mail"))))

(-> (Maybe. {:first-name "John"
             :email "foo@example.com"})
    (fmap :email)
    (fmap validate-email-try)
    (chain either->maybe)
    .value)

(-> (Maybe. {:first-name "John"
             :email "fooexample.com"})
    (fmap :email)
    (fmap validate-email-try)
    (chain either->maybe)
    .value)

(-> (Maybe. {:first-name "John"})
    (fmap :email)
    (fmap validate-email-try)
    (chain either->maybe)
    .value)


;;;;;;;;;;;;;;

(defn maybe->either
  [maybe]
  (if (nil? (.value maybe))
    (Left. "no value")
    ;; (Right. (flattenval maybe)) ;;; flattenval does not work
    (Right. (.value maybe))))

(.value (maybe->either (Maybe. "some value")))
(.value (maybe->either (Maybe. nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defn increment
;;   [v]
;;   (if (number? v) (inc v) nil))

;; (increment 1)
;; (increment "S")

;; (defn number-box
;;   [x]
;;   {:map (fn [fun] (number-box (fun x)))
;;    :value x})

;; (defn number-box
;;   [x]
;;   {:map (fn [fun]
;;           (if (number? x)
;;             (number-box (fun x))
;;             (number-box nil)))
;;    :value x})

;; (->> (number-box 5)
;;      :map
;;      (#(% (partial * 2)))
;;      :map
;;      (#(% inc))
;;      :value)

;; (->> (number-box "5")
;;      :map
;;      (#(% (partial * 2)))
;;      :map
;;      (#(% inc))
;;      :value)

;; (defn maybe
;;   [value]
;;   {:map (fn [fun]
;;           (if (nil? value)
;;             (maybe nil)
;;             (maybe (fun value))))
;;    :value value})

;; (def user {:name "Holmes" :address {:street "Baker Street" :number "221B"}})
;; (def homeless {:name "Holmes"})

;; (->> (maybe user)
;;      :map (#(% :address))
;;      :map (#(% :street))
;;      :value)

;; (->> (maybe homeless)
;;      :map (#(% :address))
;;      :map (#(% :street))
;;      :value)

;; ((((((maybe user) :map) :address) :map) :street) :value)



(defrecord Circle [radius])
;; (defrecord Square [length width])

;; ;; multimethod to calculate the area of a shape
;; (defmulti area class)
;; (defmethod area Circle [c]
;;   (* Math/PI (.radius c) (.radius c)))
;; (defmethod area Square [s]
;;   (* (.length s) (.width s)))

;; ;; create a couple shapes and get their area
(def myCircle (Circle. 10))
(println myCircle)
(.radius myCircle)
;; (def mySquare (Square. 5 11))

;; (area myCircle)
;; (area mySquare)