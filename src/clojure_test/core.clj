(ns clojure-test.core
  (:gen-class)
  (:refer-clojure)
  (:require [clojure.string :as string]
            [com.rpl.specter :as specter :refer [transform]]
            [puget.printer :as puget]
            [thoughts.core :as wtf :refer [answer]]
            [swiss.arrows :refer :all]
            [clojure.math.numeric-tower :as m]
            [clojure.core.match :refer [match]]
            [clojure.spec :as s]
            ))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Fuck you very very much, World!")
  (println "Hello, World!"))

(defn map-nn [f col]
  (remove nil? (map f col)))


(defn within
  [low high number]
  (and (>= number low)(<= number high))) 

(defn atom? [x]
  (and (not (seq? x))
       (not (vector? x))
       (not (nil? x))))


(defn member? [x col]
  (cond
    (empty? col) false
    :else (or (identical? (first col) x)
              (member? x (rest col)))))

(declare listify)

(defn contains-g? [t l]
  (not (empty? (filter some? (map #(some #{%} l) (listify t))))))

(defn listify [t]
  (if (list? t) t (list t)))

(defn slice [coll beg end]
  (let [end (if (number? end) end (count coll))]
    (-<> (drop (dec beg) coll)
         (take (- end (dec beg)) <>))))


(defn map-nested [f coll & {:keys [res] :or {res []} }]
  (cond
    (empty? coll) res
    (atom? (first coll))(map-nested f (rest coll) :res (cons (f (first coll)) res))
    :else (map-nested f (first coll) :res res)
    ))

(defn rmap [f coll]
  (clojure.walk/prewalk #(if (atom? %)(f %) %) coll))

;; (defn pwalk-c [f coll]
;;   (clojure.walk/prewalk #(if (coll? %)(f %) %) coll))

(def Y (fn [f]
         ((fn [x]
            (x x))
          (fn [x]
            (f (fn [y]
                 ((x x) y)))))))

(defn find-thing [needle haystack]
  (keep-indexed #(when (= %2 needle) %1) haystack))

(def not-member? (complement member?))

(defn two-in-a-row [coll]
  (let [[a b] (slice coll 1 2)]
    (cond
      (empty? coll) false
      (= a b) true
      :else (recur (slice coll 2 :end)))))

(defn two-in-a-row [coll]
  (let [[a b & res] coll ]
    (cond
      (empty? coll) false
      (= a b) true
      :else (recur (rest coll))
      ))) 

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
    (str "got " x)))

(def test-condp
  (fn*
   ([x]
    (let*
        [pred__33365 = expr__33366 x]
        (if (pred__33365 0 expr__33366)
          "got 0"
          (if (pred__33365 1 expr__33366) "got 1" (str "res is " x)))))))
(test-condp 0)

;; (def != (complement =))
;; (!= 1 7)

;; (defn some-fn [foo bar=42 baz=7]
;;   (+ foo bar baz))
;; (some-fn 1 2 3)
;; (some-fn 1 :bar 2)
;; (some-fn 1)
