(ns clojure-test.core
  (:gen-class)
  (:refer-clojure)
  (:require [cats.core :as m :refer [alet fapply mappend mlet]]
            [cats.monad
             [exception :as exc]
             [maybe :as maybe :refer [just nothing]]]
            [cemerick.pomegranate :refer [add-dependencies]]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as math]
            [com.rpl.specter :as specter :refer [transform walker select]]
            [puget.printer :as puget]
            [special.core :refer [condition manage]]
            [swiss.arrows :refer :all]
            [thoughts.core :as wtf :refer [answer]]))

(def != (complement =))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Fuck you very very much, World!")
  (println "Hello, World!"))

(puget/pprint 7)
(math/abs 7)

(defn listify [t]
  (if (coll? t) (into [] t) [t]))

(defn sum [values]
  (apply + values))

(defn find-thing [needle haystack]
  (keep-indexed #(when (= %2 needle) %1) haystack))

(defn map-nn [f col] (remove nil? (map f col)))

(defn within
  [low high number]
  (and (>= number low) (<= number high)))

(defn atom? [x]
  (and (not (seq? x))
       (not (vector? x))
       (not (nil? x))))

(defn member? [x col]
  (cond
    (empty? col) false
    :else (or (identical? (first col) x)
              (member? x (rest col)))))

;; (listify 7)
;; (listify [7])

(defn contains-item? [t l]
  (if (not-empty (find-thing t l)) true false))

(contains-item? "fuck" [1 2 3 4 "fuck"])
(find-thing 4 [1 2 3 4 "fuck"])


(defn slice [coll beg end]
  (let [end (if (number? end) end (count coll))]
    (-<> (drop (dec beg) coll)
         (take (- end (dec beg)) <>))))

(defn map-nested [f coll & {:keys [res] :or {res []}}]
  (cond
    (empty? coll) res
    (atom? (first coll)) (map-nested f (rest coll) :res (cons (f (first coll)) res))
    :else (map-nested f (first coll) :res res)))

(defn rmap [f coll]
  (clojure.walk/prewalk #(if (atom? %) (f %) %) coll))

;; (defn pwalk-c [f coll]
;;   (clojure.walk/prewalk #(if (coll? %)(f %) %) coll))

(def Y (fn [f]
         ((fn [x]
            (x x))
          (fn [x]
            (f (fn [y]
                 ((x x) y)))))))


(def not-member? (complement member?))

;; (defn two-in-a-row [coll]
;;   (let [[a b] (slice coll 1 2)]
;;     (cond
;;       (empty? coll) false
;;       (= a b) true
;;       :else (recur (slice coll 2 :end)))))

(defn two-in-a-row [coll]
  (let [[a b & res] coll]
    (cond
      (empty? coll) false
      (= a b) true
      :else (recur (rest coll)))))

;; (defn two-in-a-row? [l]
;;   (cond
;;     (< (count l) 2) false
;;     (apply = (slice l 1 2)) true
;;     :else (two-in-a-row? (slice l 2 :end))))
(any? 7)
(two-in-a-row [1 2 3 3 5])
(two-in-a-row [1 2 2])
(two-in-a-row [1 1 2])
(two-in-a-row [])
(two-in-a-row [1 2 3 4])
(two-in-a-row [1 2 3 3 4])

(defn test-condp [x]
  (condp = x
    0 "got 0"
    1 "got 1"
    (str "else branch, got " x)))

(test-condp 17)

;; (defn i [mv]
;;   (m/bind mv identity))

;; (defn mightbe []
;;   (let [n (rand-int 11)]
;;     (if (> n 5)
;;       (just n)
;;       (nothing))))

;; (mlet [a (mightbe)
;;        b (just (inc a))]
;;       (m/return (* b 2)))
;; (i (mlet [a (mightbe)
;;           b (just (inc a))]
;;          (m/return (* b 2))))
;; (m/mappend (just [1 2 3])
;;            (just [4 5 6]))



;; (-> (alet [a (just 1)
;;            b (mightbe)]
;;           (+ a b))
;;     (i))

;; (let [a 1
;;       b 41]
;;   (+ a b))

;; (i (fapply (just inc) (just 7)))

;; (defn m-div
;;   [x y]
;;   (if (zero? y)
;;     (maybe/nothing)
;;     (maybe/just (/ x y))))

;; (i (m/foldm m-div 1 [1 0 3]))
;; => #<Just 1/6>

;; (reduce / [1 0 3])


;; (i (m/foldm m-div 1 [1 2 3]))
;; (i (m/foldm m-div 1 [1 0 3]))
;; (i (exc/try-or-else (+ 1 nil) 42))
;; => #<Nothing>

;; (m/fmap inc [(just 1) 2 3])
;; (add-d)

(defn possible-cond [n]
  (cond
    (>= n 8) (condition :greater n :normally #(* 1000 %))
    (odd? n) (condition :odd n :normally #(* 2 %))
    (even? n) (condition :even n :normally #(* 5 %))))

(defn managed [] (map possible-cond (range 12)))

(managed)

((manage managed :odd #(+ 100 %)))

(defn plus100 [n]
  (+ 100 n))

(defn manager [f]
  ((manage managed :odd f)))

(manager plus100)
(manager #(+ 200 %))

((manage managed :odd #(+ 100 %) :greater identity))

;; (+ "fish")

;; (let [f (fn [n]
;;           (for [i (range n)]
;;             (if (odd? i)
;;               (condition :odd i :normally #(* 2 %))
;;               i)))
;;       g (manage f :odd #(+ % 100))] 
;;   (g 10))

;; (!= 1 7)

;; (defn some-fn [foo bar=42 baz=7]
;;   (+ foo bar baz))
;; (some-fn 1 2 3)
;; (some-fn 1 :bar 2)
;; (some-fn 1)

(def colleen
  {:name "Colleen"
   :pet {:name "Ebony"
         :mother {:name "Fluffy"
                  :owner {:name "Sarah"}}}})

(def richard {:name "Richard"})
(defn get-breeder-name [owner]
  ((((owner :pet) :mother) :owner) :name))

(defn get-breeder-name [owner]
  (-> owner :pet :mother :owner :name))

(or (get-breeder-name colleen) "Unknown")
(or (get-breeder-name richard) "Unknown")

(def colin
  {:name "Colin"
   :clients [{:name "Fred"
             :investment-types [{:type :gold
                                 :markets [{:name "Japan"
                                            :value 7000}

                                           {:name "America"
                                            :value 1000
                                            :bullshit 7
                                            }]}
                                {:type :shares
                                 :markets [{:name "China"
                                            :value 3000}]}]}]})

(colin (-> :clients) :investment-types)

(map :value  (mapcat :markets (mapcat :investment-types (colin :clients))))

(->> (colin :clients)
     (mapcat :investment-types)
     (mapcat :markets)
     (map :value)
     (apply +))

(transform [:outer :max] (fn [n](+ 9 n)) {:outer {:max 30 :min 10}})

((juxt :a :b) {:a 1 :b 2})
(filter identity [nil 7 nil])

;; (defn extract
;;   ([key coll]
;;    (if (coll? key)
;;      (map #(extract % coll) key)
;;      (->> (select (walker key) coll)
;;           (map key))))
;;   ([key coll f]
;;    (reduce f (extract key coll))))

(defn extract-value [coll key]
  (into [] (map key (select (walker key) coll))))

(defn extract-combine [coll & keys]
  (flatten (map #(extract-value coll %) keys)))

;; (defn extract [coll targets]
;;   (if (= (count targets) 1)
;;     (extract-value coll (first targets))
;;     (extract (extract-value coll (first targets)) (rest targets))))

(defn extract [coll targets]
  (let [t (listify targets)]
    (if (= (count t) 1)
      (extract-value coll (first t))
      (extract (extract-value coll (first t)) (rest t)))))

(defn extract [coll targets]
  (let [t (listify targets)]
    (if (empty? t) coll
      (extract (extract-value coll (first t)) (rest t)))))


(extract colin [:markets :name])
(extract colin [:value])
(extract colin :value)
(into [] (extract colin :value))
(reduce + (extract-combine colin :value :bullshit))
(reduce + (extract colin :value))
(extract  (extract colin :markets) :name)
(-> (extract colin :markets)
     (extract :name))
(select (walker :value) colin)
(map :name (select (walker :value) colin))

(defn call-unless-nil [f v]
  (if (nil? v) nil
      (f v)))

(defn flip [f]
  (fn [& args] (apply f (reverse args))))

(defn wrap-last [wrapper steps]
  (let [last (last steps)
        wrapped (comp wrapper last)]
    (replace {last wrapped} steps)))

(fapply (just inc) (nothing))

(call-unless-nil inc 7)

(def nil-chainer {:step-runner call-unless-nil
                  :inner-wrapper identity})

(def list-chainer {:step-runner mapcat
                   :inner-wrapper list})

(defn chain
  [{:keys [step-runner inner-wrapper]} value steps]
  (reduce (flip step-runner)
          value
          (wrap-last inner-wrapper steps)))

(chain nil-chainer colleen [:pet :mother :owner :name])
(chain nil-chainer richard [:pet :mother :owner :name])
(chain list-chainer (colin :clients) [:investment-types :markets :value])
(chain list-chainer (richard :clients) [:investment-types :markets :value])
