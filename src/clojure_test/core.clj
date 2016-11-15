(ns clojure-test.core
  (:gen-class)
  (:refer-clojure
   )
  (:require [clojure.string :as string]
            [com.rpl.specter :as specter :refer [transform]]
            [puget.printer :as puget]
            [thoughts.core :as wtf :refer [answer]]
            [swiss.arrows :refer :all]
            )
  
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Fuck you very very much, World!")
  (println "Hello, World!"))

(defn p [arg]
  (puget/cprint arg))


(defn any? [pred col] (not (not-any? pred col)))

(defn error-message
  [severity]
  (str "OH GOD! IT'S A DISASTER! WE'RE "
       (if (= severity :mild)
         "MILDLY INCONVEINIENCED!"
         "FUCKED!")))

(defn alt-error-message
  [severity]
  (str "OH GOD! IT'S A DISASTER! WE'RE "
       (cond (= severity :mild)
             "MILDLY INCONVEINIENCED!"
             (= severity :dire)
             "VERY FUCKED!"
             true
             "DOOOOOOMED!")))

(alt-error-message :mild)

(alt-error-message :mild)
(alt-error-message :other)
(alt-error-message :dire)
(error-message :mild)
(error-message :other)
(error-message :dire)
(defn beg [col]
  (take (-(count col)1) col))
(def myvec [1 2 3 4 5])
(last myvec)
(beg myvec)
(defn arity
  "an example of different arities"
  ([]
   (str "no args!"))
  ([arg]
   (str "one arg and it is " arg))
  ([arg1 arg2]
   (str "two args and they are " arg1 " " arg2)))
(defn x-chop
  "describe chop"
  ([name chop-type]
   (str "I " chop-type " chop " name "! Take that!"))
  ([name]
   (x-chop name "death"))
  ([]
   (x-chop "bob" "karate")))
(defn beat-group
  [chop-type & names]
  (map #(x-chop %1 chop-type) names))
(x-chop "bob")
(beat-group "death" "bob" "jan" "fred")
(apply (partial beat-group "slap") [ "bob" "jane"])
(defn announce-treasure-location
  [{lat :lat lng :lng}]
  (println (str "Treasure lat: " lat))
  (println (str "Treasure lng: " lng)))

(defn announce-treasure-location2
  [{:keys [lat lng] :as treasure-location}]
  (println (str "Treasure lat: " lat))
  (println (str "Treasure lng: " lng))
  treasure-location)

;================================================================
;                hobbit related stupidity                       =
;================================================================
(def asym-hobbit-body-parts [{:name "head"           :size 3}
                             {:name "left-eye"       :size 1}
                             {:name "left-ear"       :size 1}
                             {:name "mouth"          :size 1}
                             {:name "nose"           :size 1}
                             {:name "neck"           :size 2}
                             {:name "left-shoulder"  :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest"          :size 10}
                             {:name "back"           :size 10}
                             {:name "left-forearm"   :size 3}
                             {:name "abdomen"        :size 6}
                             {:name "left-kidney"    :size 1}
                             {:name "left-nut"       :size 1}
                             {:name "left-hand"      :size 2}
                             {:name "left-knee"      :size 2}
                             {:name "left-thigh"     :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles"  :size 1}
                             {:name "left-foot"      :size 2}])

(defn matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn both-parts
  [part]
  (if (.contains (:name part) "left")
    (list part (matching-part part))
    part))

(defn alt-both-parts
  [part]
  (into [] (set [part (matching-part part)])))

(defn reduce-to-symetry
  "Expects a seq of maps that have a :name and :size"
  [parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set [part (matching-part part)])))
          []
          parts))

(defn simpler-symetry
  "Expects a seq of maps what have a :name and :size"
  [parts]
  (into [] (flatten (map both-parts parts))))


(defn simpler-symetry2
  "Expects a seq of maps what have a :name and :size"
  [parts]
  (flatten (into [] (map both-parts parts))))

(defn symetrize-body-parts
  "Expects a seq of maps what have a :name and :size"
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
                     (set [part (matching-part part)])))))))


(def hobbit-body (symetrize-body-parts asym-hobbit-body-parts))                            
(reduce-to-symetry hobbit-body)

(defn hit-hobbit
  [parts]
  (let [sym-parts (reduce-to-symetry asym-hobbit-body-parts)
        body-part-size-sum (reduce + (map :size sym-parts))
        target (rand body-part-size-sum)]
    (loop [[part & remaining] sym-parts accumulated-size (:size part)]
      (if (> accumulated-size target)
        (:name part)
        (recur remaining (+ accumulated-size (:size (first remaining))))))))

(defn mapset
  [f coll]
  (into #{} (map f coll)))

(def identities
  [{:alias "Batman"       :real "Bruce Wayne"  }
   {:alias "Spider-man"   :real "Peter Parker" }
   {:alias "Santa"        :real "Your mom"     }
   {:alias "Easter Bunny" :real "Your dad"     }])

(def food-journal
  [{:month 1 :day 1 :human 5.3 :critter 2.3}
   {:month 1 :day 2 :human 5.1 :critter 2.0}
   {:month 2 :day 1 :human 4.9 :critter 2.1}
   {:month 2 :day 2 :human 5.0 :critter 2.5}
   {:month 3 :day 1 :human 4.2 :critter 3.3}
   {:month 3 :day 2 :human 4.0 :critter 3.8}
   {:month 4 :day 1 :human 3.7 :critter 3.9} 
   {:month 4 :day 2 :human 3.7 }])


(defn max-consumed
  [key]
  (if (any? #(contains? % key) food-journal)
    (apply max (filter #(not (nil? %)) (map #(key %) food-journal)))
    0))

(do (println  (max-consumed :human)
              (max-consumed :critter)
              (max-consumed :bullshit)))


(defn within
  [low high number]
  (and (>= number low)(<= number high))) 

(filter #(within 2 3 (:month %1)) food-journal)

(defn print-journal-date
  [date]
  (select-keys date [:day :month]))

(defn print-journal-date2
  [{:keys [day month] :as bullshit}]
  (println bullshit)
  {:day day :month month})

(defn bob [] (println "fuck you very much"))
(defn sam [] (println "wtf"))
(bob) 
(sam) 
(bob) 
(println 3 (println 7))

(defn head [col]
  (take (- (count col) 1) col))

(defn rev [col] (if (empty? col)
               nil
               (cons (last col) (rev (head col)))))

;; (defn revr
;;   ([remaining] (revr remaining '()))
;;   ([remaining res] (if (empty? remaining)
;;                res
;;                (recur (head remaining)(concat res [(last remaining)])))))

;; (defn revr2
;;   ([col] (revr2 col '()))
;;   ([col acc] (if (empty? col)
;;                acc
;;                (let [remaining (head col)
;;                      result (concat acc [(last col)])]
;;                  (recur remaining result)))))

(defn rev
  ([col] (rev col []))
  ([col acc] (if (empty? col)
               acc
               (let [remaining (rest col)
                     result (concat [(first col)] acc)]
                 (recur remaining result)))))

(rev '(1 2 3 4 5))



(defn intl
  ([cols] (intl cols []))
  ([cols acc] (if (some empty? cols)
               acc
               (let [remaining (map rest cols)
                     result (concat acc (map first cols))]
                 (recur remaining result)))))
(intl [[1 2 3] [4 5]])


;;;-------------------------------------------------------
(def filename "suspects.csv")

(defn str->int [str] (Integer. str))

(defn split-csv [string]
  (map #(string/split % #",")
       (string/split string #"\n")))

;; (map process-entry (split-csv (slurp filename)))

(defn process-entry [entry]
  {:name (first entry) :glitter-index (str->int (last entry))})

(defn parse [file]
  (->> file slurp split-csv (map process-entry)))

(p (parse filename))

(defn glitter-filter [min]
  (filter #(>= (:glitter-index %) min) (parse filename)))

(println (glitter-filter 0))
(defn foobar
  "not a real function"
  [something & something-else]
  (+ 2 6 (+ 7 2))
  (str something "this" something-else))

(defn barfoo
  [something]
  (+ 2 6)
  (+ 7 9)
  (+ 7 23))

(foobar "fuck")
(barfoo "fuck")
(def blarg 9)
(str (+ 2 blarg))

(+ 2 2 (+ 2 7 9 8) 2 5 2)

(reduce (fn [new-map [key val]]
          (assoc new-map key (inc val)))
        {}
        {:max 30 :min 10})

(defn my-updater [f my-map]
  (reduce (fn [new-map [key val]]
            (assoc new-map key (f val)))
          {}
          my-map))

(defmacro infix
  "infix macro"
  [infixed]
  (list (second infixed)
        (first infixed)
        (last infixed)))

(infix (1 + 1 (infix (1 + 3))))
(infix (1 + 3))
(my-updater #(+ % 3) {:max 30 :min 10})
(transform [:outer :max] (fn [n](+ 9 n)) {:outer {:max 30 :min 10}})
(-<> 2
     (* <> 5)
     (vector 1 2 <> 3 4))
(puget/cprint hobbit-body)
(p hobbit-body)
(+ 1 1)
(defn xform [xs]
  (-<> xs
     (map #(+ 2 %) <>)
     (filter odd? <>)
     (apply + <>)))

(xform (range 0 10))

(defn atom? [x]
  (and (not (seq? x))
       (not (nil? x))))

(defn lat? [l]
  (every? atom? l))

(defn member? [x col]
  (cond
    (empty? col) false
    :else (or (identical? (first col) x)
              (member? x (rest col)))))
(defn rember [x col]
  (cond
    (empty? col) '()
    (identical? (first col) x) (rest col)
    :else (cons (first col)(rember x (rest col)))))

(def ls [[:apple :peach :pumkin][:plum :pear :cherry]])
