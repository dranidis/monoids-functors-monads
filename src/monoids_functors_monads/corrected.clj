(ns monoids-functors-monads.corrected)

(defprotocol Functor
  (fmap [j f]))


;; (deftype Nothing
;;          []
;;   Functor
;;   (fmap [_ _] (Nothing.)))


(deftype Maybe [value]
  Functor
  (fmap [_ f] (if (nil? value)
                (Maybe. nil)
                (Maybe. (f value)))))

(.value (fmap (Maybe. nil) inc))
(.value (Maybe. nil))

(defmulti nothing? class)
;; (defmethod nothing? Nothing [_] true)
(defmethod nothing? Maybe [m] (nil? (.value m)))

(defmulti get-or-else (fn [m _] (class m)))
(defmethod get-or-else nil [_ default-value] default-value)

(defmethod get-or-else Maybe
  [maybe default-value]
  (if (nil? (.value maybe))
    default-value
    (.value maybe)))

;; (defmethod get-or-else Nothing
;;   [_ default-value]
;;   default-value)


(deftype Right [value]
  Functor
  (fmap [_ f] (Right. (f value))))

(deftype Left [value]
  Functor
  (fmap [_ _] (Left. value)))

;; catch

(defmulti catch (fn [m _] (class m)))
(defmethod catch Right [m _] (Right. (.value m)))
(defmethod catch Left [m f] (Right. (f (.value m))))

;; flatten

(defmulti flattenval (fn [m] (class m)))

(defmethod flattenval Maybe [m] (.value m))
;; (defmethod flattenval Nothing [_] (Nothing.))
(defmethod flattenval Right [m] (.value m))
(defmethod flattenval Left [m] (.value m))


(defmulti chain (fn [m _] (class m)))
(defmethod chain Maybe [m f] (-> m (fmap f) flattenval))
;; (defmethod chain Nothing [_ _] (Nothing.))
(defmethod chain Right [m f] (-> m (fmap f) flattenval))
(defmethod chain Left [m _] (.value m))


(defn try-catch
  [fun]
  (fn [value]
    (try (Right. (fun value))
         (catch Exception e (Left. e)))))

(defn maybe->either
  [maybe]
  (if (nothing? maybe)
    (Left. "no value")
    (Right. (flattenval maybe))))

(defn either->maybe
  [either]
  (Maybe. (-> either (catch (fn [_] nil)) flattenval)))

(def validate-email
  (try-catch (fn [value]
               (if (re-matches #"\S+@\S+\.\S+" value)
                 value
                 (throw (Exception. "Invalid mail"))))))

(defn parse-mail
  [user]
  (-> (Maybe. user)
      (fmap :email)
      (fmap (fn [mail]
              (-> mail validate-email (catch (fn [e] (.getMessage e))))))
      (chain either->maybe)
      (get-or-else "no email")))


(prn (parse-mail {:first-name "John"
                  :email "foo@example.com"}))

(prn (parse-mail {:first-name "John"
                  :email "fooexample.com"}))

(prn (parse-mail {:first-name "John"}))
