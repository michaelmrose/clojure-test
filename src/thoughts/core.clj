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

;; (defn pair? [x]
;;   (and
;;    (coll? x)
;;    (= 2 (count (flatten x)))))

;; (defn pair?
;;   ([x]
;;    (pair? x 0))
;;   ([x n]
;;    (let [n (inc n)
;;          f (first x)
;;          r (rest x)]
;;      (cond
;;        (empty? r) n
;;        (atom? f) (pair? r n)
;;        :else (+ (pair? (first f) n))))))

;; (defn pair? [x]
;;   (cond
;;     (empty? x) 0
;;     (atom? (first x)) (+ 1 (pair? (rest x)))
;;     :else (+ (pair? [(first (first x))]) (pair? (rest (first x))))))

;; (defn mycount [x]
;;   (if (atom? x) 1
;;       (count x)))

;; (defn recursive-count [coll]
;;   (let [atoms (filter atom? coll)
;;         colls (filter coll? coll)])
;;   (cond
;;     (lat? coll) (count coll)
;;     :else (+ (count atom?) (recursive-count coll))))

;; (recursive-count [1 2 3 [4 5]])

;; (defn car [v]
;;   (cond
;;     (string? v) (str (first v))
;;     (coll? v) (first v)))

;; (defn cdr [v]
;;   (cond
;;     (string? v) (string/join (rest v))
;;     (coll? v) (rest v)))
;; (defn p [arg]
;;   (puget/cprint arg))


;; (defn flatapply [f & items]
;;   (apply f (flatten (list items)))
;;   )
;; (defn mapflat [f l]
;;   (flatten (map f l)))
;; (defn anyof? [pred col] (not (not-any? pred col)))

;; (defn beg [col]
;;   (take (-(count col)1) col))
;; (defn rev
;;   ([col] (rev col []))
;;   ([col acc] (if (empty? col)
;;                acc
;;                (let [remaining (rest col)
;;                      result (concat [(first col)] acc)]
;;                  (recur remaining result)))))

;; (rev '(1 2 3 4 5))



;; (defn intl
;;   ([cols] (intl cols []))
;;   ([cols acc] (if (some empty? cols)
;;                acc
;;                (let [remaining (map rest cols)
;;                      result (concat acc (map first cols))]
;;                  (recur remaining result)))))


;; (defn intl2 [cols]
;;   (loop [cols cols acc []]
;;     (if (some empty? cols)
;;       acc
;;       (let [remaining (map rest cols)
;;             result (concat acc (map first cols))]
;;         (recur remaining result)))))

;; (defn intl3 [& cols]
;;   (loop [acc [] c cols]
;;     (if (every? empty? c)
;;       acc
;;       (recur  (concat acc (map-nn first c))
;;               (map rest c)))))


;; (intl [[1 2 3] [4 5]])
;; (intl2 [[1 2 3] [4 5]])
;; (intl3 [[1 2 3] [4 5] [7]])
;; (intl3 [1 2 3] [4 5] [7 8])


;; ;;;-------------------------------------------------------


;; (reduce (fn [new-map [key val]]
;;           (assoc new-map key (inc val)))
;;         {}
;;         {:max 30 :min 10})

;; (defn my-updater [f my-map]
;;   (reduce (fn [new-map [key val]]
;;             (assoc new-map key (f val)))
;;           {}
;;           my-map))

;; (defmacro infix
;;   "infix macro"
;;   [infixed]
;;   (list (second infixed)
;;         (first infixed)
;;         (last infixed)))

;; (infix (1 + 1 (infix (1 + 3))))
;; (infix (1 + 3))
;; (my-updater #(+ % 3) {:max 30 :min 10})
;; (transform [:outer :max] (fn [n](+ 9 n)) {:outer {:max 30 :min 10}})
;; (-<> 2
;;      (* <> 5)
;;      (vector 1 2 <> 3 4))
;; (defn xform [xs]
;;   (-<> xs
;;      (map #(+ 2 %) <>)
;;      (filter odd? <>)
;;      (apply + <>)))

;; (xform (range 0 10))


;; (defn lat? [l]
;;   (cond
;;     (empty? l) true
;;     (atom? (first l))(lat? (rest l))
;;     :else false))

;; (defn rember [x col]
;;   (cond
;;     (empty? col) '()
;;     (identical? (first col) x) (rest col)
;;     :else (cons (car col)(rember x (cdr col)))))

;; (defn firsts [col]
;;   (if (every? not-empty col)
;;     (map first col)
;;     false))
;; (defn firstsr [l]
;;   (cond
;;     (empty? l) false
;;     :else (cons
;;            (car (car l))
;;            (firstsr (cdr l)))))

;; (def ls [[:apple :peach :pumkin][:plum :pear :cherry]])
;; (def lsn [[1 2][3 4][][5 6]])

;; (defn insert-r-if-match [item old new]
;;   (if (identical? item old)
;;     (list old new)
;;     (list item)))

;; (defn insert-l-if-match [item old new]
;;   (if (identical? item old)
;;     (list new old)
;;     (list item)))
;; (defn insert-r [old new l]
;;   (cond
;;     (not (member? old l)) nil
;;     (identical? (first l) old) (concat (list old new)(rest l))
;;     :else (cons (first l) (insert-r old new (rest l)))))

;; (defn multi-insert-r [old new l]
;;   (cond
;;     (not (member? old l)) nil
;;     (coll? (first l)) (cons (first l)(multi-insert-r (rest l)))
;;     (identical? (first l) old) (concat (list old new)(multi-insert-r old new (rest l)))
;;     :else (cons (first l) (multi-insert-r old new (rest l)))))

;; (defn multi-insert-r [old new l]
;;   (cond
;;     (empty? l) nil
;;     :else (concat
;;            (insert-r-if-match (first l) old new)
;;            (multi-insert-r old new (rest l)))))

;; (defn mins-r [old new l]
;;   (flatten (map #(insert-r-if-match % old new) l)))

;; (defn mins-l [old new l]
;;   (flatten (map #(insert-l-if-match % old new) l)))

;; (multi-insert-r 2 7 [1 2 3 2 4 2 5])
;; (mins-r 2 7 [1 2 3 2 4 2 5])
;; (mins-l 2 7 [1 2 3 2 4 2 5])

;; (defn seqR [v old new]
;;   (if (= v old)
;;     [old new]
;;     [v]))
;; (defn seqL [v old new]
;;   (if (= v old)
;;     [new old]
;;     [v]))

;; (defn subsinner [v old new]
;;   (if (= v old)
;;     [new]
;;     [v]))

;; (subsinner 1 2 7)

;; (defn insert-d2 [old new l f]
;;   (if
;;     (empty? l) '()
;;     (concat (f (first l) old new) (insert-d2 old new (rest l) f))))

;; (defn insert-l2 [old new l]
;;   (insert-d2 old new l seqL))
;; (defn insert-r2 [old new l]
;;   (insert-d2 old new l seqR))

;; (defn insert-d3 [f]
;;   (fn inner [old new l]
;;     (if
;;         (empty? l) '()
;;         (concat (f (first l) old new) (inner old new (rest l))))))

;; (def insert-r3 (insert-d3 seqR))
;; (def insert-l3 (insert-d3 seqL))
;; (def subst2 (insert-d3 subsinner))
;; (subst2 2 7 [1 2 3])


;; (insert-r3 2 7 [1 2 3])
;; (insert-l3 2 7 [1 2 3])

;; ;; (insert-d2 2 7 [1 2 3] seqL)
;; (defn insert-d [m l]
;;   (cond
;;     (not (member? (:old m) l)) false
;;     (identical? (first l) (:old m)) (concat (vals m)(rest l))
;;     :else (cons (first l) (insert-d m (rest l)))))

;; (insert-d {:new 7 :old 2} [1 2 3])

;; (defn subst [new old l]
;;   (cond
;;     (empty? l) '()
;;     (contains-g? old (list (car l)))(cons new (cdr l))
;;     :else (cons (car l))))

;; (defn ne [t]
;;   (not (empty? t)))




;; ;; (defn contains-g? [t l]
;; ;;   (-> some?
;; ;;       (filter (map #(some #{%} l) (listify t)))
;; ;;       ne))



;; (contains-g? 7 [1 2 7])
;; (contains-g? '(7 2) '(1 2 7))

;; (defn mrember [t l]
;;   (cond
;;     (empty? l) '()
;;     (not (contains-g? t l)) l
;;     :else (mrember t (rember t l))
;;     ))

;; (mrember 2 '(1 2 3 2 3 2 5))
;; ;; (subst 4 3 '(1 2 3))

;; (insert-d {:old 4 :new 8} '(1 2 4))
;; (insert-r 1 2 '(1 3 4))
;; (insert-r 1 2 '())
;; (insert-r 4 5 '(1 2 4))
;; (insert-r 7 8 '(1 2 3))
;; (member? :apple [:peach :apple :pear])
;; (rember :apple [:peach :apple :pear])

;; (def cards
;;   (for  [suit [:heart :spade :diamond :club]
;;          num [:ace 2 3 4 5 6 7 8 9 10 :jack :queen :king]]
;;     [num suit]))
;; (-<> cards
;;      (shuffle <>)
;;      (take 10 <>)
;;      (group-by second <>)
;;      (:heart <>))

;; (defn plus [n1 n2]
;;   (cond
;;     (zero? n2) n1
;;     :else (plus (inc n1) (dec n2))))

;; (defn sub [n1 n2]
;;   (cond
;;     (zero? n2) n1
;;     :else (sub (dec n1) (dec n2))))

;; (defn tup? [col]
;;   (or
;;    (empty? col)
;;    (every? number? col)))

;; (defn sumtup [t]
;;   (cond
;;     (< (count t) 2) t)
;;   :else (sumtup (plus (first t (first (rest t))))))

;; (defn solo [col]
;;   (= (count col) 1))

;; (defn sumtup [t]
;;   (cond
;;     (solo t) t
;;     :else (sumtup (cons (apply + (take 2 t)) (drop 2 t)))))

;; ;; (defn mply [n m]
;; ;;   (cond
;; ;;     (zero? m) 0
;; ;;     :else (+ n (mply n (dec m)))))


;; (defn addtups [t1 t2]
;;   (map #(+ %1 %2) t1 t2))

;; (def ls [[1 2 3][4 5 6][1 1 1]])
;; (defn addtupsm [ts]
;;   (reduce (fn [t1 t2] (map #(+ %1 %2) t1 t2)) ts))

;; (reduce addtups ls)

;; (defn non-nils [col]
;;   (filter identity col))


;; (defn addtupsm [ts]
;;   (cond
;;     (solo ts) ts
;;     :else (flatten (addtupsm (cons (apply addtups (take 2 ts))(drop 2 ts))))))

;; (defn addtupsm [ts]
;;   (cond
;;     (every? empty? ts) nil
;;     :else (cons (apply + (non-nils (map first ts)))
;;                 (addtupsm (non-nils (map rest ts))))))

;; (defn addtupsm [ts]
;;   (cond
;;     (every? empty? ts) nil
;;     :else (into [] (cons (apply + (map-nn first ts))
;;                          (addtupsm (map-nn rest ts))))))


;; (addtupsm [[1 2 3] [4 5 6][1 1]])
;; ;; (addtupsm [(range 10000)(range 10000)])

;; (for [x (range 40)
;;             :when (= 1 (rem x 4))]
;;         x)
;; (for [[x y] (partition 2 (range 20))]
;;         (+ x y))

;; (defn exp [x n]
;;   (reduce * (repeat n x)))

;; (defn first-str [s]
;;   (str (first s)))

;; (defn rest-str [s]
;;   (string/join (rest s)))

;; (defn str->bin [string]
;;   (if (= "0" string) 0 1))

;; (def file "fuck.txt")

;; (defn readbin[s]
;;   (cond
;;    	(empty? s) 0
;;     (not (re-matches #"[01]*" s)) nil
;;     :else 
;;     (-<> (count s)
;;          (dec <>)
;;          (m/expt 2 <>)
;;          (* (str->bin (car s)) <>)
;;          (+ <> (readbin (cdr s))))))

;; (readbin "100111")

;; (map readbin (string/split-lines (slurp file)))

;; (defn defval [& x]
;;   (let [x (if x x 10)]
;;     x))

;; (defn defval2 [valuezero & {:keys [valueone valuetwo] :or {valueone 42 valuetwo 43}}]
;;   [valuezero valueone valuetwo])

;; ;; (defval)
;; (defval2 7 :valueone 1)

;; (defn matchtest [x y z]
;;   (match [x y z]
;;          [_ false true] 1
;;          [false true _ ] 2
;;          [_ _ false] 3
;;          [_ _ true] 4
;;          :else 5))

;; (matchtest true true true)
;; (matchtest true true false)
;; (defn cancelneg [col]
;;   (if (anyof? neg? col)
;;     (let [m (apply min col)]
;;       (map #(+ (m/abs m) %) col))
;;     col))
;; (cancelneg [-3 -7])
;; (defn my> [x y]
;;   (let [[x y] (cancelneg [x y])]
;;        (cond
;;          (= x y) false
;;          (zero? y) true
;;          (zero? x) false
;;          :else (my> (dec x)(dec y)))))

;; (cancelneg [-3 3])
;; (my> 5 5)

;; (+ 7 4)
;; (defn pick [col n & {:keys [index] :or {index 1}}]
;;   (cond
;;     (= index n)(first col)
;;     :else (pick (rest col) n :index (inc index))))

;; ;; (defn slice [coll beg end]
;; ;;   (take (inc (- end beg)) (drop (dec beg) coll)))

;; (defn pick [col n & {:keys [index] :or {index 1}}]
;;   (if (= index n)
;;     (first col)
;;     (pick (rest col) n :index (inc index))))

;; (defn rempick [col n & {:keys [index begining] :or {index 1 begining []}}]
;;   (if (= index n)
;;     (concat begining (rest col))
;;     (rempick (rest col) n :index (inc index) :begining (conj begining (first col)))))

;; (defn rempick2 [col n]
;;   (concat (take (dec n) col) (drop n col)))

;; (defn rempick3 [coll n]
;;   (concat (slice coll 1 (dec n))
;;           (slice coll (inc n) (count coll))))



;; (defn countocc [coll target & { :keys [count] :or {count 0}}]
;;   (cond
;;     (empty? coll) count
;;     (= target (first coll)) (countocc (rest coll) target :count (inc count))
;;     :else (countocc (rest coll) target :count count)))

;; (countocc [1 2 7 7 7 3 5 7] 7)

;; (slice [1 2 3 4 5] 2 4)

;; (rempick [1 2 3 4 5] 3)
;; (rempick2 [1 2 3 4 5] 3)
;; (rempick3 [1 2 3 4 5] 3)
;; (rempick3 [1 2 3 4 5] 1)

;; (pick [5 6 7] 4)

;; (defn rember* [coll target & {:keys [res] :or {res []}}]
;;   (cond
;;     (empty? coll) '()
;;     (atom? (first coll)) (if (= target (first coll))
;;                            (rember* (rest coll) target)
;;                            (cons (first coll) (rember* (rest coll) target)))
    
;;     :else (cons  (rember* (first coll) target)
;;                  (rember* (rest coll) target))))

;; (defn rember* [coll target]
;;   (cond
;;     (empty? coll) '()
;;     (atom? (first coll)) (if (= target (first coll))
;;                            (rember* (rest coll) target)
;;                            (cons (first coll) (rember* (rest coll) target)))
    
;;     :else (cons  (rember* (first coll) target)
;;                  (rember* (rest coll) target))))


;; (rember* '(7 1 2 7 3 4 7) 7)
;; (rember* '((7 1 2 (1 2 7)3 7) 4 7) 7)
;; (first '((4 5 6)1 2 3))

;; (defn insertr* [coll target new]
;;   (cond
;;     (empty? coll) '()
;;     (atom? (first coll))(if (= target (first coll))
;;                           (concat (list (first coll) new) (insertr* (rest coll) target new))
;;                           (cons (first coll) (insertr* (rest coll) target new)))
;;     :else (cons (insertr* (first coll) target new)
;;                 (insertr* (rest coll) target new))))

;; ;; (defn occur* [coll target & {:keys [n] :or {n 0}} ]
;; ;;   (cond
;; ;;     (empty? coll) n
;; ;;     (atom? (first coll))(if (= target (first coll))
;; ;;                           (occur* (rest coll) target :n (inc n))
;; ;;                           (occur* (rest coll) target :n n))
;; ;;     :else (+ (occur* (first coll) target :n n)
;; ;;              (occur* (rest coll) target))))

;; (defn occur* [coll target & {:keys [n] :or {n 0}} ]
;;   (cond
;;     (empty? coll) n
;;     (atom? (first coll))(if (= target (first coll))
;;                           (occur* (rest coll) target :n (inc n))
;;                           (occur* (rest coll) target :n n))
;;     :else (+ (occur* (first coll) target :n n)
;;              (occur* (rest coll) target))))

;; (defn occur* [coll target]
;;   (cond
;;     (empty? coll) 0
;;     (atom? (first coll))(if (= target (first coll))
;;                           (inc  (occur* (rest coll) target))
;;                           (occur* (rest coll) target))
;;     :else (+ (occur* (first coll) target)
;;              (occur* (rest coll) target))))
;; (occur* [] 3)
;; (occur* [1 2 3 [2 3 4 [2 3][3 5][3]]] 3)

;; (defn member* [coll target]
;;   (cond
;;     (empty? coll) false
;;     (atom? (first coll)) (if (= target (first coll))
;;                            true
;;                            (member* (rest coll) target))
;;     :else (if (member* (first coll) target)
;;             true
;;             (member* (rest coll) target))))

;; (member* [1 2 3 [4 5 [6 [7 8]]]] 9)

;; (member* [1 2 3 [4 5 [6 [7 8]]]] 7)
;; (insert-r-if-match 2 2 7)

;; (insertr* '(2 1 2 3 2 (2 2)) 2 7)

;; (def nested-list '((7 1 2 (1 2 7 (1 2 3 7))3 7) 4 7))
;; (first (first nested-list))

;; ;; (map-nested inc '(1 2 3))
;; ;; (map-nested inc '(1 (3 4) 2 3))

;; (defn atom? [v]((complement coll?) v))


;; (defn replacement [target item]
;;   (if-not (= target item)
;;     item))

;; (defn rember-simple [coll target]
;;   (map-nn #(replacement target %) coll))

;; (rember-simple '(1 2 7) 7)
;; (rember-simple '(7 1 2 3 7 4 5 7) 7)

;; (defn cons-nn [x seq]
;;   (if-not (nil? x)
;;     (if (seq? seq)
;;       (cons x seq)
;;       (cons x (cons seq '())))
;;     seq))

;; (defn conj-nn [col x]
;;   (if-not (nil? x)
;;     (conj col x)
;;     seq))

;; (cons-nn nil [1 2 3])

;; (pwalk-c #(rember-simple % 7) nested-list)
;; (rmap inc [1 2 [4 5] 3])
;; (pwalk-c #(multi-insert-r 2 7 %) '(2 (2 4 (5 2))))
;; (rmap #(inc %) '(2 (2 4 (5 2))))

;; (defn constrained-fn [f x]
;;   {:pre  [(pos? x)
;;           (> x 3)]
;;    :post [(= % (* 2 x))]}
;;   (f x))

;; (constrained-fn #(* 2 %) 4)

;; (defn leftmost [coll]
;;   (cond
;;     (empty? coll) nil
;;     (atom? (first coll)) (first coll)
;;     :else (leftmost (first coll))))

;; (leftmost [[[0 9] 1 [7 8]] 2 3])

;; (= [[0 1] 2 3] [[0 1] 2 4])

;; (defn myset? [coll]
;;   (cond
;;     (empty? coll) true
;;     (member? (first coll) (rest coll)) false
;;     :else (myset? (rest coll))))
;; ;; (defn myset? [l]
;; ;;   (cond
;; ;;    (empty? l) true
;; ;;    (and (not-member? (first l) (rest l)) (myset? (rest l))) true
;; ;;    :else false))

;; (defn makeset [coll]
;;   (cond
;;     (empty? coll) '()
;;     (member? (first coll)
;;              (rest coll))(makeset (rest coll))
;;     :else (cons (first coll)(makeset (rest coll)))
;;     ))

;; (set [1 1 2 3 1 3])

;; (defn makeset2 [coll]
;;   (cons (first coll)
;;         (makeset
;;          (mrember (first coll)
;;                   (rest coll)))))

;; ;; (time (dotimes [n 300] (makeset [1 1 2 3 1 3 7])))
;; ;; (time (dotimes [n 300] (makeset2 [1 1 2 3 1 3 7])))
;; ;; (time (makeset2 [1 1 2 3 1 3 7]))

;; (defn subset? [coll1 coll2]
;;   (or (empty? coll1)
;;     (and
;;      (member? (first coll1) coll2)
;;      (subset? (rest coll1) coll2))))

;; (defn eqset? [coll1 coll2]
;;   (= (set coll1) (set coll2)))

;; (defn eqset? [c1 c2]
;;   (cond
;;     (every? empty? [c1 c2]) true
;;     (some empty? [c1 c2]) false
;;     :else (eqset? (rest c1) (rember (first c1) c2))))

;; (defn eqset? [c1 c2]
;;   (and (subset? c1 c2)
;;        (subset? c2 c1)))

;; (defn eqset? [c1 c2]
;;   (= (sort c1) (sort c2)))

;; (subset? [1 4] [1 2 3 4 5])
;; (subset? [1 7 4] [1 2 3 4 5])
;; (myset? [2 1 2 3])

;; (eqset? [1 2 3 4 5] [5 4 3 2 1])
;; (eqset? [1 7] [1 7 2])

;; (defn intersect? [c1 c2]
;;   (cond
;;     (some empty? [c1 c2]) false
;;     (member? (first c1) c2) true
;;     :else (intersect? (rest c1) c2)))

;; (defn intersect? [c1 c2]
;;   (and (every? not-empty [c1 c2])
;;        (or (member? (first c1) c2)
;;            (intersect? (rest c1) c2))))

;; (defn intersection
;;   ([c1]  c1)
;;   ([c1 c2]
;;    (intersection c1 c2 []))
;;   ([c1 c2 res]
;;    (let [f (first c1)
;;          r (rest c1)]
;;      (cond
;;        (some empty? [c1 c2]) res
;;        (member? f c2) (intersection r c2 (conj res f))
;;        :else (intersection r c2 res)))))

;; ;; {:keys [valueone valuetwo] :or {valueone 42 valuetwo 43}}
;; ;; (defn intersection [c1 c2 & {:keys [res] :or {res []}}]
;; ;;   (let [f (first c1)
;; ;;         r (rest c1)]
;; ;;     (cond
;; n;;       (some empty? [c1 c2]) res
;; ;;       (member? f c2) (intersection r c2 :res (conj res f))
;; ;;       :else (intersection r c2 :res res))))

;; ;; (defn intersection [c1 c2]
;; ;;   (let [f (first c1)
;; ;;         r (rest c1)]
;; ;;     (cond
;; ;;       (some empty? [c1 c2]) '()
;; ;;       (member? f c2) (cons f (intersection r c2))
;; ;;       :else (intersection r c2))))

;; (defn union [c1 c2]
;;   (makeset (concat c1 c2)))

;; (intersection [2 3 4 5 7][3 4 5 6])
;; (intersection '(2 3 4 5 7)'(3 4 5 6))


;; (union [1 2 3 4] [3 4 5 6])
;; (union [:stewed :tomatos :and :macaroni :caserole] [:macaroni :and :cheese])


;; (intersect? [7 2 1] [2 3 4])
;; (intersect? [1 2] [3 4 5])
;; (intersect? [0 1 2] [1 2 3])
;; ;; (defn inverse-of-membership [x seq]
;; ;;   (if (member? x seq)
;; ;;     nil
;; ;;     x))

;; (defn member [x col]
;;   (if (member? x col) x nil))


;; (defn not-member [x col]
;;   (if (member? x col) nil x))

;; (defn set-difference [c1 c2]
;;   (cond
;;     (empty? c1) '()
;;     (member? (first c1) c2)(set-difference (rest c1) c2)
;;     :else (cons (first c1) (set-difference (rest c1) c2))))

;; ;; (defn set-difference [c1 c2 & {:keys [res] :or {res []}}]
;; ;;   (cond
;; ;;     (empty? c1) '()
;; ;;     :else (cons-nn (not-member (first c1) c2) (set-difference (rest c1) c2))))

;; (defn set-difference
;;   ([c1 c2]
;;    (set-difference c1 c2 []))
;;   ([c1 c2 res]
;;    (if (empty? c1) res
;;        (recur (rest c1) c2 (cons-nn (not-member (first c1) c2) res)))))

;; (defn set-difference
;;   ([c1 c2]
;;    (set-difference c1 c2 []))
;;   ([c1 c2 res]
;;    (let [val (not-member (first c1) c2)]
;;      (if (empty? c1) res
;;          (recur (rest c1) c2 (if (not (nil? val))
;;                                (conj res val)
;;                                res))))))
;; (defn set-difference
;;   ([c1 c2]
;;    (into [] (filter identity (set-difference c1 c2 []))))
;;   ([c1 c2 res]
;;    (let [val (not-member (first c1) c2)]
;;      (if (empty? c1) res
;;          (recur (rest c1) c2 
;;                 (conj res val))))))

;; (defn set-difference [c1 c2]
;;   (map-nn #(not-member % c2) c1))

;; (defn set-difference [c1 c2]
;;   (filter identity (map #(not-member % c2) c1)))

;; (set-difference [1 2 3 4] [3 4 5 6])
;; (set-difference  (range 1000)(range 999))

;; ;; (time (dotimes [n 10] (set-difference (range 10) (range 8)) ))

;; (defn intersect-all [coll]
;;   (let [c (intersection (first coll) (second coll))
;;         r (slice coll 3 :end)]
;;     (if (empty? r)
;;       c
;;       (recur (cons c r)))))

;; (defn intersect-all [coll]
;;   (if
;;     (empty? (rest coll)) (first coll)
;;     (intersection (first coll) (intersect-all (rest coll)))))

;; (defn intersect-all [coll]
;;   (reduce intersection coll))

;; (intersect-all [[45 7 2] [7 2 8] [7 2 9]])
;; (intersect-all [[7] [7 2 8] [7 2 9]])
;; (intersect-all [(range 40 50) (range 45) (range 40 43)])
;; ;; (time (dotimes [n 100]
;; ;;         (intersect-all [(range 40 50) (range 45) (range 40 43)])))
;; (intersection [45 7] [7])

;; (defn pair? [coll]
;;   (cond
;;     (empty? coll) false
;;     (atom? coll) false
;;     (empty? (rest coll)) false
;;     (empty? (rest (rest coll))) true
;;     :else false))

;; (defn rcount [coll]
;;   (apply +  (flatten (rmap (fn [a] 1) coll))))

;; ;; (defn pair? [coll]
;; ;;   (= 2 (rcount coll)))

;; (rcount [1 2 [3 4]])


;; ;; (defn pair? [x]
;; ;;   (and
;; ;;    (coll? x)
;; ;;    (= 2 (count (flatten x)))))

;; ;; (defn pair?
;; ;;   ([x]
;; ;;    (pair? x 0))
;; ;;   ([x n]
;; ;;    (let [n (inc n)
;; ;;          f (first x)
;; ;;          r (rest x)]
;; ;;      (cond
;; ;;        (empty? r) n
;; ;;        (atom? f) (pair? r n)
;; ;;        :else (+ (pair? (first f) n))))))

;; ;; (defn pair? [x]
;; ;;   (cond
;; ;;     (empty? x) 0
;; ;;     (atom? (first x)) (+ 1 (pair? (rest x)))
;; ;;     :else (+ (pair? [(first (first x))]) (pair? (rest (first x))))))
;; (defn rel? [l]
;;   (every? pair? l))


;; (myset? [1 1 2])

;; (defn fun? [l]
;;   (and
;;    (rel? l)
;;    (myset? (map first l))))

;; (defn fullfun? [l]
;;   (and
;;    (rel? l)
;;    (every? myset? [(map first l) (map second l)])))

;; (defn funlookup [fun val]
;;   (cond
;;     (not (fun? fun)) nil
;;     (empty? fun) nil
;;     (= val (first (first fun))) (second (first fun))
;;     :else (funlookup (rest fun) val))
;;   )

;; (defn add-to-table [name val table]
;;   (conj table [name val]))

;; (-<> [[3 "three"] [4 "four"]]
;;      (add-to-table 5 "five" <> )
;;      (funlookup <> 5))
;; (let [t [[3 "three"][ 4 "four"]]
;;       newtable (add-to-table 5 "five" t)
;;       res (funlookup newtable 5)]
;;   res)

;; (funlookup (add-to-table 5 "five" [[3 "three"] [4 "four"]]) 5)
;; (fun? [[1 2][3 4]])
;; (fullfun? [[1 2][3 4]])
;; (fullfun? [[1 2][3 2]])
;; (funlookup [[1 2][3 4][:test "fuck"]] :test)
;; (funlookup [[1 2][3 4][:test]] :test)
;; (funlookup [[1 2][3 4][:test "fuck"]] :testme)

;; ;; (defn mymap [f coll]
;; ;;   (cond
;; ;;     (empty? coll) '()
;; ;;     (every? atom? coll) (cons (f (first coll)) (mymap f (rest coll)))
;; ;;     :else (rmap f coll)))

;; (rmap #(+ 7 %) [1 2 3[4 5]])

;; (println "fuck")

;; (defn lookup-in-entry [identifiers values x]
;;   (cond
;;     (some empty? [identifiers values]) nil
;;     (not (= (count identifiers) (count values))) nil
;;     (not-every? myset? [identifiers values]) nil
;;     :else (nth values (first (find-thing x identifiers)))
;;     )
;;   )

;; (lookup-in-entry [1 2 3]["one" "two" "three"] 3)
