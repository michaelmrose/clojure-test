(ns thoughts.core
  (:gen-class)
  (:require [clojure.string :as string]))

(def answer 42)
;; ((defn error-message
;;   [severity]
;;   (str "OH GOD! IT'S A DISASTER! WE'RE "
;;        (if (= severity :mild)
;;          "MILDLY INCONVEINIENCED!"
;;          "FUCKED!")))

;; (defn alt-error-message
;;   [severity]
;;   (str "OH GOD! IT'S A DISASTER! WE'RE "
;;        (cond (= severity :mild)
;;              "MILDLY INCONVEINIENCED!"
;;              (= severity :dire)
;;              "VERY FUCKED!"
;;              true
;;              "DOOOOOOMED!")))

;; (alt-error-message :mild)

;; (alt-error-message :mild)
;; (alt-error-message :other)
;; (alt-error-message :dire)
;; (error-message :mild)
;; (error-message :other)
;; (error-message :dire)
;; def question "unknown")
;; (defmacro $1
;;   "creates a generic function like intl wherein you pass in only the values
;;    bound in the let which are used in the recur"
;;   ([cols] (list '$ cols []))
;;   ([cols acc] (list 'if ('some 'empty? cols) cols)))

;; ($ [] [7])
;; (defmacro mymacro
;;   "creates a generic function like intl wherein you pass in only the values
;;    bound in the let which are used in the recur"
;;   [cols & bindings]
;;   (list 'if ('some 'empty? cols)  acc 17)
;;   ;; basically the idea is to return a function wherein
;;   ;; you bind all bindings for each iteration then call recur with the values
;;   ;; in order of receipt you would then be able to define intl in terms of it
;;   ;; like so
;;   (defn intl
;;     [cols]
;;     ($ [remaining (map rest cols)]
;;        [result (concat acc (map first cols))]))

;;   )

;; (mymacro [] 7)


;; (defn even-numbers
;;   ([] (even-numbers 0))
;;   ([n] (cons n (lazy-seq (even-numbers (+ n 2))))))

;; (defn odd-numbers
;;   ([] (odd-numbers 1))
;;   ([n] (cons n (lazy-seq (odd-numbers (+ n 2))))))

;; (defn sum
;;   ([vals]
;;    (sum vals 0))
;;   ([vals accumulating-total]
;;    (println vals)
;;    (println accumulating-total)
;;    (if (empty? vals)
;;      accumulating-total
;;      (recur (rest vals) (+ (first vals) accumulating-total)))))


;; (intl [1 2 3] [4 5 6])

;; (defn intlnew [col]
;;   (mymacro [remaining (map rest cols)
;;             result (concat acc ((map first cols)))]))



;; (infix (1 + 2))
;; (defn arity
;;   "an example of different arities"
;;   ([]
;;    (str "no args!"))
;;   ([arg]
;;    (str "one arg and it is " arg))
;;   ([arg1 arg2]
;;    (str "two args and they are " arg1 " " arg2)))
;; (defn x-chop
;;   "describe chop"
;;   ([name chop-type]
;;    (str "I " chop-type " chop " name "! Take that!"))
;;   ([name]
;;    (x-chop name "death"))
;;   ([]
;;    (x-chop "bob" "karate")))
;; (defn beat-group
;;   [chop-type & names]
;;   (map #(x-chop %1 chop-type) names))
;; (x-chop "bob")
;; (beat-group "death" "bob" "jan" "fred")
;; (apply (partial beat-group "slap") [ "bob" "jane"])
;; (defn announce-treasure-location
;;   [{lat :lat lng :lng}]
;;   (println (str "Treasure lat: " lat))
;;   (println (str "Treasure lng: " lng)))

;; (defn announce-treasure-location2
;;   [{:keys [lat lng] :as treasure-location}]
;;   (println (str "Treasure lat: " lat))
;;   (println (str "Treasure lng: " lng))
;;   treasure-location)

;; ;================================================================
;; ;                hobbit related stupidity                       =
;; ;================================================================
;; (def asym-hobbit-body-parts [{:name "head"           :size 3}
;;                              {:name "left-eye"       :size 1}
;;                              {:name "left-ear"       :size 1}
;;                              {:name "mouth"          :size 1}
;;                              {:name "nose"           :size 1}
;;                              {:name "neck"           :size 2}
;;                              {:name "left-shoulder"  :size 3}
;;                              {:name "left-upper-arm" :size 3}
;;                              {:name "chest"          :size 10}
;;                              {:name "back"           :size 10}
;;                              {:name "left-forearm"   :size 3}
;;                              {:name "abdomen"        :size 6}
;;                              {:name "left-kidney"    :size 1}
;;                              {:name "left-nut"       :size 1}
;;                              {:name "left-hand"      :size 2}
;;                              {:name "left-knee"      :size 2}
;;                              {:name "left-thigh"     :size 4}
;;                              {:name "left-lower-leg" :size 3}
;;                              {:name "left-achilles"  :size 1}
;;                              {:name "left-foot"      :size 2}])

;; (defn matching-part
;;   [part]
;;   {:name (clojure.string/replace (:name part) #"^left-" "right-")
;;    :size (:size part)})

;; (defn both-parts
;;   [part]
;;   (if (.contains (:name part) "left")
;;     (list part (matching-part part))
;;     part))

;; (defn alt-both-parts
;;   [part]
;;   (into [] (set [part (matching-part part)])))

;; (defn reduce-to-symetry
;;   "Expects a seq of maps that have a :name and :size"
;;   [parts]
;;   (reduce (fn [final-body-parts part]
;;             (into final-body-parts (set [part (matching-part part)])))
;;           []
;;           parts))

;; (defn simpler-symetry
;;   "Expects a seq of maps what have a :name and :size"
;;   [parts]
;;   (into [] (flatten (map both-parts parts))))


;; (defn simpler-symetry2
;;   "Expects a seq of maps what have a :name and :size"
;;   [parts]
;;   (flatten (into [] (map both-parts parts))))

;; (defn symetrize-body-parts
;;   "Expects a seq of maps what have a :name and :size"
;;   [asym-body-parts]
;;   (loop [remaining-asym-parts asym-body-parts final-body-parts []]
;;     (if (empty? remaining-asym-parts)
;;       final-body-parts
;;       (let [[part & remaining] remaining-asym-parts]
;;         (recur remaining
;;                (into final-body-parts
;;                      (set [part (matching-part part)])))))))


;; (def hobbit-body (symetrize-body-parts asym-hobbit-body-parts))                            
;; (reduce-to-symetry hobbit-body)

;; (defn hit-hobbit
;;   [parts]
;;   (let [sym-parts (reduce-to-symetry asym-hobbit-body-parts)
;;         body-part-size-sum (reduce + (map :size sym-parts))
;;         target (rand body-part-size-sum)]
;;     (loop [[part & remaining] sym-parts accumulated-size (:size part)]
;;       (if (> accumulated-size target)
;;         (:name part)
;;         (recur remaining (+ accumulated-size (:size (first remaining))))))))

;; (defn mapset
;;   [f coll]
;;   (into #{} (map f coll)))

;; (def identities
;;   [{:alias "Batman"       :real "Bruce Wayne"  }
;;    {:alias "Spider-man"   :real "Peter Parker" }
;;    {:alias "Santa"        :real "Your mom"     }
;;    {:alias "Easter Bunny" :real "Your dad"     }])

;; (def food-journal
;;   [{:month 1 :day 1 :human 5.3 :critter 2.3}
;;    {:month 1 :day 2 :human 5.1 :critter 2.0}
;;    {:month 2 :day 1 :human 4.9 :critter 2.1}
;;    {:month 2 :day 2 :human 5.0 :critter 2.5}
;;    {:month 3 :day 1 :human 4.2 :critter 3.3}
;;    {:month 3 :day 2 :human 4.0 :critter 3.8}
;;    {:month 4 :day 1 :human 3.7 :critter 3.9} 
;;    {:month 4 :day 2 :human 3.7 }])


;; (defn max-consumed
;;   [key]
;;   (if (any? #(contains? % key) food-journal)
;;     (apply max (filter #(not (nil? %)) (map #(key %) food-journal)))
;;     0))

;; (do (println  (max-consumed :human)
;;               (max-consumed :critter)
;;               (max-consumed :bullshit)))

;; (def heists [{:location "Cologne, Germany"
;;               :cheese-name "Archbishop's Cheese Pretzel"
;;               :lat 50.95
;;               :lng 6.97}
;;              {:location "Zurich, Switzerland"
;;               :cheese-name "The Standard Emmental"
;;               :lat 47.37
;;               :lng 8.55
;;               }
;;              {:location "Marsiele, France"
;;               :cheese-name "Le Fromage de Cosquer"
;;               :lat 43.30
;;               :lng 5.37
;;               }
;;              {:location "Zurich, Switzerland"
;;               :cheese-name "The Lesser Emmental"
;;               :lat 47.37
;;               :lng 8.55
;;               }
;;              {:location "Vatican City"
;;               :cheese-name "The Cheese of Turin"
;;               :lat 41.90
;;               :lng 12.45
;;               }])

;; Return to this later
;; (defn comparator-over-maps
;;   [comparison-fn ks]
;;   (fn [maps]
;;     (zipmap ks
;;             (map (fn [k] (apply comparison-fn (map k maps)))
;;                  ks))))

;; (def min (comparator-over-maps clojure.core/min [:lat :lng]))
;; (def max (comparator-over-maps clojure.core/max [:lat :lang]))

;; (min [{:a 1 :b 3} {:a 5 :b 0}])

;; (defn lat? [l]
;;   (cond
;;     (empty? l) true
;;     (atom? (first l))(lat? (rest l))
;;     :else false))

;; (defn lat-next [l]
;;   (if (or (empty? l)
;;           (and (atom? (first l))
;;                (lat? (rest l))))
;;     true
;;     false))


;; (def xform
;;   (comp
;;     (map #(+ 2 %))
;;     (filter odd?)))
;; (transduce xform + 0 (range 0 10))
;; (xform 0 1)

;; (defn rev [col] (if (empty? col)
;;                nil
;;                (cons (last col) (rev (head col)))))

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

;; (defn print-journal-date
;;   [date]
;;   (select-keys date [:day :month]))

;; (defn print-journal-date2
;;   [{:keys [day month] :as bullshit}]
;;   (println bullshit)
;;   {:day day :month month})

;; (def filename "suspects.csv")

;; (defn str->int [str] (Integer. str))

;; (defn split-csv [string]
;;   (map #(string/split % #",")
;;        (string/split string #"\n")))

;; (map process-entry (split-csv (slurp filename)))

;; (defn process-entry [entry]
;;   {:name (first entry) :glitter-index (str->int (last entry))})

;; (defn parse [file]
;;   (->> file slurp split-csv (map process-entry)))

;; (p (parse filename))

;; (defn glitter-filter [min]
;;   (filter #(>= (:glitter-index %) min) (parse filename)))

;; (println (glitter-filter 0))
;; (puget/cprint hobbit-body)
;; (p hobbit-body)

;; (filter #(within 2 3 (:month %1)) food-journal)
