(ns clojure-test.core
  (:gen-class)
  (:refer-clojure)
  (:require [clojure.string :as string]
            ;; [clojure.core :refer [first rest] :rename {first car rest cdr}]
            [com.rpl.specter :as specter :refer [transform]]
            [puget.printer :as puget]
            [thoughts.core :as wtf :refer [answer]]
            [swiss.arrows :refer :all]
            [clojure.math.numeric-tower :as m]
            [clojure.core.match :refer [match]]
            ))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Fuck you very very much, World!")
  (println "Hello, World!"))

(defn map-nn [f col]
  (remove nil? (map f col)))

(defn car [v]
  (cond
    (string? v) (str (first v))
    (coll? v) (first v)))

(defn cdr [v]
  (cond
    (string? v) (string/join (rest v))
    (coll? v) (rest v)))
(defn p [arg]
  (puget/cprint arg))


(defn flatapply [f & items]
  (apply f (flatten (list items)))
  )
(defn mapflat [f l]
  (flatten (map f l)))
(defn any? [pred col] (not (not-any? pred col)))

(defn beg [col]
  (take (-(count col)1) col))


(defn within
  [low high number]
  (and (>= number low)(<= number high))) 

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


(defn intl2 [cols]
  (loop [cols cols acc []]
    (if (some empty? cols)
      acc
      (let [remaining (map rest cols)
            result (concat acc (map first cols))]
        (recur remaining result)))))

(defn intl3 [& cols]
  (loop [acc [] c cols]
    (if (every? empty? c)
      acc
      (recur  (concat acc (map-nn first c))
              (map rest c)))))


(intl [[1 2 3] [4 5]])
(intl2 [[1 2 3] [4 5]])
(intl3 [[1 2 3] [4 5] [7]])
(intl3 [1 2 3] [4 5] [7 8])


;;;-------------------------------------------------------


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
  (cond
    (empty? l) true
    (atom? (first l))(lat? (rest l))
    :else false))

(lat? [1 2 [1 2]])
(atom? [1 2])
(seq? '(1 2))

(defn member? [x col]
  (cond
    (empty? col) false
    :else (or (identical? (first col) x)
              (member? x (rest col)))))
(defn rember [x col]
  (cond
    (empty? col) '()
    (identical? (first col) x) (rest col)
    :else (cons (car col)(rember x (cdr col)))))

(defn firsts [col]
  (if (every? not-empty col)
    (map first col)
    false))
(defn firstsr [l]
  (cond
    (empty? l) false
    :else (cons
           (car (car l))
           (firstsr (cdr l)))))

(def ls [[:apple :peach :pumkin][:plum :pear :cherry]])
(def lsn [[1 2][3 4][][5 6]])

(defn insert-r-if-match [item old new]
  (if (identical? item old)
    (list old new)
    (list item)))

(defn insert-l-if-match [item old new]
  (if (identical? item old)
    (list new old)
    (list item)))
(defn insert-r [old new l]
  (cond
    (not (member? old l)) nil
    (identical? (first l) old) (concat (list old new)(rest l))
    :else (cons (first l) (insert-r old new (rest l)))))

(defn multi-insert-r [old new l]
  (cond
    (not (member? old l)) nil
    (coll? (first l)) (cons (first l)(multi-insert-r (rest l)))
    (identical? (first l) old) (concat (list old new)(multi-insert-r old new (rest l)))
    :else (cons (first l) (multi-insert-r old new (rest l)))))

(defn multi-insert-r [old new l]
  (cond
    (empty? l) nil
    :else (concat
           (insert-r-if-match (first l) old new)
           (multi-insert-r old new (rest l)))))

(defn mins-r [old new l]
  (flatten (map #(insert-r-if-match % old new) l)))

(defn mins-l [old new l]
  (flatten (map #(insert-l-if-match % old new) l)))

(multi-insert-r 2 7 [1 2 3 2 4 2 5])
(mins-r 2 7 [1 2 3 2 4 2 5])
(mins-l 2 7 [1 2 3 2 4 2 5])

(defn insert-d [m l]
  (cond
    (not (member? (:old m) l)) false
    (identical? (first l) (:old m)) (concat (vals m)(rest l))
    :else (cons (first l) (insert-d m (rest l)))))



(declare listify)

(defn contains-g? [t l]
  (not (empty? (filter some? (map #(some #{%} l) (listify t))))))

(defn listify [t]
  (if (list? t) t (list t)))

(defn subst [new old l]
  (cond
    (empty? l) '()
    (contains-g? old (list (car l)))(cons new (cdr l))
    :else (cons (car l))))

(defn ne [t]
  (not (empty? t)))




;; (defn contains-g? [t l]
;;   (-> some?
;;       (filter (map #(some #{%} l) (listify t)))
;;       ne))



(contains-g? 7 [1 2 7])
(contains-g? '(7 2) '(1 2 7))

(defn mrember [t l]
  (cond
    (empty? l) '()
    (not (contains-g? t l)) l
    :else (mrember t (rember t l))
    ))

(mrember 2 '(1 2 3 2 3 2 5))
;; (subst 4 3 '(1 2 3))

(insert-d {:old 4 :new 8} '(1 2 4))
(insert-r 1 2 '(1 3 4))
(insert-r 1 2 '())
(insert-r 4 5 '(1 2 4))
(insert-r 7 8 '(1 2 3))
(member? :apple [:peach :apple :pear])
(rember :apple [:peach :apple :pear])

(def cards
  (for  [suit [:heart :spade :diamond :club]
         num [:ace 2 3 4 5 6 7 8 9 10 :jack :queen :king]]
    [num suit]))
(-<> cards
     (shuffle <>)
     (take 10 <>)
     (group-by second <>)
     (:heart <>))

(defn plus [n1 n2]
  (cond
    (zero? n2) n1
    :else (plus (inc n1) (dec n2))))

(defn sub [n1 n2]
  (cond
    (zero? n2) n1
    :else (sub (dec n1) (dec n2))))

(defn tup? [col]
  (or
   (empty? col)
   (every? number? col)))

(defn sumtup [t]
  (cond
    (< (count t) 2) t)
  :else (sumtup (plus (first t (first (rest t))))))

(defn solo [col]
  (= (count col) 1))

(defn sumtup [t]
  (cond
    (solo t) t
    :else (sumtup (cons (apply + (take 2 t)) (drop 2 t)))))

(defn mply [n m]
  (cond
    (zero? m) 0
    :else (+ n (mply n (dec m)))))


(defn addtups [t1 t2]
  (map #(+ %1 %2) t1 t2))

(def ls [[1 2 3][4 5 6][1 1 1]])
(defn addtupsm [ts]
  (reduce (fn [t1 t2] (map #(+ %1 %2) t1 t2)) ts))

(reduce addtups ls)

(defn non-nils [col]
  (filter identity col))


(defn addtupsm [ts]
  (cond
    (solo ts) ts
    :else (flatten (addtupsm (cons (apply addtups (take 2 ts))(drop 2 ts))))))

(defn addtupsm [ts]
  (cond
    (every? empty? ts) nil
    :else (cons (apply + (non-nils (map first ts)))
                (addtupsm (non-nils (map rest ts))))))

(defn addtupsm [ts]
  (cond
    (every? empty? ts) nil
    :else (into [] (cons (apply + (map-nn first ts))
                         (addtupsm (map-nn rest ts))))))


(addtupsm [[1 2 3] [4 5 6][1 1]])
;; (addtupsm [(range 10000)(range 10000)])

(for [x (range 40)
            :when (= 1 (rem x 4))]
        x)
(for [[x y] (partition 2 (range 20))]
        (+ x y))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn first-str [s]
  (str (first s)))

(defn rest-str [s]
  (string/join (rest s)))

(defn str->bin [string]
  (if (= "0" string) 0 1))

(def file "fuck.txt")

(defn readbin[s]
  (cond
   	(empty? s) 0
    (not (re-matches #"[01]*" s)) nil
    :else 
    (-<> (count s)
         (dec <>)
         (m/expt 2 <>)
         (* (str->bin (car s)) <>)
         (+ <> (readbin (cdr s))))))

(readbin "100111")

(map readbin (string/split-lines (slurp file)))

(defn defval [& x]
  (let [x (if x x 10)]
    x))

(defn defval2 [valuezero & {:keys [valueone valuetwo] :or {valueone 42 valuetwo 43}}]
  [valuezero valueone valuetwo])

;; (defval)
(defval2 7 :valueone 1)

(defn matchtest [x y z]
  (match [x y z]
         [_ false true] 1
         [false true _ ] 2
         [_ _ false] 3
         [_ _ true] 4
         :else 5))

(matchtest true true true)
(matchtest true true false)
(defn cancelneg [col]
  (if (any? neg? col)
    (let [m (apply min col)]
      (map #(+ (m/abs m) %) col))
    col))
(cancelneg [-3 -7])
(defn my> [x y]
  (let [[x y] (cancelneg [x y])]
       (cond
         (= x y) false
         (zero? y) true
         (zero? x) false
         :else (my> (dec x)(dec y)))))

(cancelneg [-3 3])
(my> 5 5)

(+ 7 4)
(defn pick [col n & {:keys [index] :or {index 1}}]
  (cond
    (= index n)(first col)
    :else (pick (rest col) n :index (inc index))))

(defn slice [coll beg end]
  (let [end (if (number? end) end (count coll))]
    (-<> (drop (dec beg) coll)
         (take (- end (dec beg)) <>))))

;; (defn slice [coll beg end]
;;   (take (inc (- end beg)) (drop (dec beg) coll)))

(defn pick [col n & {:keys [index] :or {index 1}}]
  (if (= index n)
    (first col)
    (pick (rest col) n :index (inc index))))

(defn rempick [col n & {:keys [index begining] :or {index 1 begining []}}]
  (if (= index n)
    (concat begining (rest col))
    (rempick (rest col) n :index (inc index) :begining (conj begining (first col)))))

(defn rempick2 [col n]
  (concat (take (dec n) col) (drop n col)))

(defn rempick3 [coll n]
  (concat (slice coll 1 (dec n))
          (slice coll (inc n) (count coll))))



(defn countocc [coll target & { :keys [count] :or {count 0}}]
  (cond
    (empty? coll) count
    (= target (first coll)) (countocc (rest coll) target :count (inc count))
    :else (countocc (rest coll) target :count count)))

(countocc [1 2 7 7 7 3 5 7] 7)

(slice [1 2 3 4 5] 2 4)

(rempick [1 2 3 4 5] 3)
(rempick2 [1 2 3 4 5] 3)
(rempick3 [1 2 3 4 5] 3)
(rempick3 [1 2 3 4 5] 1)

(pick [5 6 7] 4)

(defn rember* [coll target & {:keys [res] :or {res []}}]
  (cond
    (empty? coll) '()
    (atom? (first coll)) (if (= target (first coll))
                           (rember* (rest coll) target)
                           (cons (first coll) (rember* (rest coll) target)))
    
    :else (cons  (rember* (first coll) target)
                 (rember* (rest coll) target))))

(defn rember* [coll target]
  (cond
    (empty? coll) '()
    (atom? (first coll)) (if (= target (first coll))
                           (rember* (rest coll) target)
                           (cons (first coll) (rember* (rest coll) target)))
    
    :else (cons  (rember* (first coll) target)
                 (rember* (rest coll) target))))


(rember* '(7 1 2 7 3 4 7) 7)
(rember* '((7 1 2 (1 2 7)3 7) 4 7) 7)
(first '((4 5 6)1 2 3))

(defn insertr* [coll target new]
  (cond
    (empty? coll) '()
    (atom? (first coll))(if (= target (first coll))
                          (concat (list (first coll) new) (insertr* (rest coll) target new))
                          (cons (first coll) (insertr* (rest coll) target new)))
    :else (cons (insertr* (first coll) target new)
                (insertr* (rest coll) target new))))

;; (defn occur* [coll target & {:keys [n] :or {n 0}} ]
;;   (cond
;;     (empty? coll) n
;;     (atom? (first coll))(if (= target (first coll))
;;                           (occur* (rest coll) target :n (inc n))
;;                           (occur* (rest coll) target :n n))
;;     :else (+ (occur* (first coll) target :n n)
;;              (occur* (rest coll) target))))

(defn occur* [coll target & {:keys [n] :or {n 0}} ]
  (cond
    (empty? coll) n
    (atom? (first coll))(if (= target (first coll))
                          (occur* (rest coll) target :n (inc n))
                          (occur* (rest coll) target :n n))
    :else (+ (occur* (first coll) target :n n)
             (occur* (rest coll) target))))

(defn occur* [coll target]
  (cond
    (empty? coll) 0
    (atom? (first coll))(if (= target (first coll))
                          (inc  (occur* (rest coll) target))
                          (occur* (rest coll) target))
    :else (+ (occur* (first coll) target)
             (occur* (rest coll) target))))
(occur* [] 3)
(occur* [1 2 3 [2 3 4 [2 3][3 5][3]]] 3)

(defn member* [coll target]
  (cond
    (empty? coll) false
    (atom? (first coll)) (if (= target (first coll))
                           true
                           (member* (rest coll) target))
    :else (if (member* (first coll) target)
            true
            (member* (rest coll) target))))

(member* [1 2 3 [4 5 [6 [7 8]]]] 9)

(member* [1 2 3 [4 5 [6 [7 8]]]] 7)
(insert-r-if-match 2 2 7)

(insertr* '(2 1 2 3 2 (2 2)) 2 7)

(def nested-list '((7 1 2 (1 2 7 (1 2 3 7))3 7) 4 7))
(first (first nested-list))

;; (defn map-nested [f coll & {:keys [res] :or {res []} }]
;;   (cond
;;     (empty? coll) res
;;     (atom? (first coll))(map-nested f (rest coll) :res (cons (f (first coll)) res))
;;     :else (map-nested f (first coll) :res res)
;;     ))

;; (map-nested inc '(1 2 3))
;; (map-nested inc '(1 (3 4) 2 3))

(defn atom? [v]((complement coll?) v))


(defn replacement [target item]
  (if-not (= target item)
    item))

(defn rember-simple [coll target]
  (map-nn #(replacement target %) coll))

(rember-simple '(1 2 7) 7)
(rember-simple '(7 1 2 3 7 4 5 7) 7)

(defn cons-nn [x seq]
  (if-not (nil? x)
    (cons x seq)
    seq))

(defn conj-nn [col x]
  (if-not (nil? x)
    (conj col x)
    seq))

(cons-nn nil [1 2 3])

(defn pwalk-a [f coll]
  (clojure.walk/prewalk #(if (atom? %)(f %) %) coll))

(defn pwalk-c [f coll]
  (clojure.walk/prewalk #(if (coll? %)(f %) %) coll))

(pwalk-c #(rember-simple % 7) nested-list)
(pwalk-a inc [1 2 [4 5] 3])
(pwalk-c #(multi-insert-r 2 7 %) '(2 (2 4 (5 2))))
(pwalk-a #(inc %) '(2 (2 4 (5 2))))

(defn constrained-fn [f x]
  {:pre  [(pos? x)
          (> x 3)]
   :post [(= % (* 2 x))]}
  (f x))

(constrained-fn #(* 2 %) 4)

(defn leftmost [coll]
  (cond
    (empty? coll) nil
    (atom? (first coll)) (first coll)
    :else (leftmost (first coll))))

(leftmost [[[0 9] 1 [7 8]] 2 3])

(= [[0 1] 2 3] [[0 1] 2 4])

(defn myset? [coll]
  (cond
    (empty? coll) true
    (member? (first coll) (rest coll)) false
    :else (myset? (rest coll))))

(defn makeset [coll]
  (cond
    (empty? coll) '()
    (member? (first coll)
             (rest coll))(makeset (rest coll))
    :else (cons (first coll)(makeset (rest coll)))
    ))

(set [1 1 2 3 1 3])

(defn makeset2 [coll]
  (cons (first coll)
        (makeset
         (mrember (first coll)
                  (rest coll)))))

(time (dotimes [n 300] (makeset [1 1 2 3 1 3 7])))
(time (dotimes [n 300] (makeset2 [1 1 2 3 1 3 7])))
(time (makeset2 [1 1 2 3 1 3 7]))

(defn subset? [coll1 coll2]
  (or (empty? coll1)
    (and
     (member? (first coll1) coll2)
     (subset? (rest coll1) coll2))))

(defn eqset? [coll1 coll2]
  (= (set coll1) (set coll2)))

(defn eqset? [c1 c2]
  (cond
    (every? empty? [c1 c2]) true
    (any? empty? [c1 c2]) false
    :else (eqset? (rest c1) (rember (first c1) c2))))

(defn eqset? [c1 c2]
  (and (subset? c1 c2)
       (subset? c2 c1)))

(defn eqset? [c1 c2]
  (= (sort c1) (sort c2)))

(subset? [1 4] [1 2 3 4 5])
(subset? [1 7 4] [1 2 3 4 5])
(myset? [2 1 2 3])

(eqset? [1 2 3 4 5] [5 4 3 2 1])
(eqset? [1 7] [1 7 2])

(defn intersect? [c1 c2]
  (cond
    (any? empty? [c1 c2]) false
    (member? (first c1) c2) true
    :else (intersect? (rest c1) c2)))

(defn intersect? [c1 c2]
  (and (every? not-empty [c1 c2])
       (or (member? (first c1) c2)
           (intersect? (rest c1) c2))))

(defn intersection
  ([c1]  c1)
  ([c1 c2]
   (intersection c1 c2 []))
  ([c1 c2 res]
   (let [f (first c1)
         r (rest c1)]
     (cond
       (any? empty? [c1 c2]) res
       (member? f c2) (intersection r c2 (conj res f))
       :else (intersection r c2 res)))))

;; {:keys [valueone valuetwo] :or {valueone 42 valuetwo 43}}
;; (defn intersection [c1 c2 & {:keys [res] :or {res []}}]
;;   (let [f (first c1)
;;         r (rest c1)]
;;     (cond
;;       (any? empty? [c1 c2]) res
;;       (member? f c2) (intersection r c2 :res (conj res f))
;;       :else (intersection r c2 :res res))))

;; (defn intersection [c1 c2]
;;   (let [f (first c1)
;;         r (rest c1)]
;;     (cond
;;       (any? empty? [c1 c2]) '()
;;       (member? f c2) (cons f (intersection r c2))
;;       :else (intersection r c2))))

(defn union [c1 c2]
  (makeset (concat c1 c2)))

(intersection [2 3 4 5 7][3 4 5 6])
(intersection '(2 3 4 5 7)'(3 4 5 6))


(union [1 2 3 4] [3 4 5 6])
(union [:stewed :tomatos :and :macaroni :caserole] [:macaroni :and :cheese])


(intersect? [7 2 1] [2 3 4])
(intersect? [1 2] [3 4 5])
(intersect? [0 1 2] [1 2 3])
;; (defn inverse-of-membership [x seq]
;;   (if (member? x seq)
;;     nil
;;     x))

(defn member [x col]
  (if (member? x col) x nil))

(defn not-member [x col]
  (if (member? x col) nil x))

(defn set-difference [c1 c2]
  (cond
    (empty? c1) '()
    (member? (first c1) c2)(set-difference (rest c1) c2)
    :else (cons (first c1) (set-difference (rest c1) c2))))

;; (defn set-difference [c1 c2 & {:keys [res] :or {res []}}]
;;   (cond
;;     (empty? c1) '()
;;     :else (cons-nn (not-member (first c1) c2) (set-difference (rest c1) c2))))

(defn set-difference
  ([c1 c2]
   (set-difference c1 c2 []))
  ([c1 c2 res]
   (if (empty? c1) res
       (recur (rest c1) c2 (cons-nn (not-member (first c1) c2) res)))))

(defn set-difference
  ([c1 c2]
   (set-difference c1 c2 []))
  ([c1 c2 res]
   (let [val (not-member (first c1) c2)]
     (if (empty? c1) res
         (recur (rest c1) c2 (if (not (nil? val))
                               (conj res val)
                               res))))))
(defn set-difference
  ([c1 c2]
   (into [] (filter identity (set-difference c1 c2 []))))
  ([c1 c2 res]
   (let [val (not-member (first c1) c2)]
     (if (empty? c1) res
         (recur (rest c1) c2 
                (conj res val))))))

(defn set-difference [c1 c2]
  (map-nn #(not-member % c2) c1))

(defn set-difference [c1 c2]
  (filter identity (map #(not-member % c2) c1)))

(set-difference [1 2 3 4] [3 4 5 6])
(set-difference  (range 1000)(range 999))

(time (dotimes [n 10] (set-difference (range 10) (range 8)) ))

(defn intersect-all [coll]
  (let [c (intersection (first coll) (second coll))
        r (slice coll 3 :end)]
    (if (empty? r)
      c
      (recur (cons c r)))))

(defn intersect-all [coll]
  (if
    (empty? (rest coll)) (first coll)
    (intersection (first coll) (intersect-all (rest coll)))))

(defn intersect-all [coll]
  (reduce intersection coll))

(intersect-all [[45 7 2] [7 2 8] [7 2 9]])
(intersect-all [[7] [7 2 8] [7 2 9]])
(intersect-all [(range 40 50) (range 45) (range 40 43)])
;; (time (dotimes [n 100]
;;         (intersect-all [(range 40 50) (range 45) (range 40 43)])))
(intersection [45 7] [7])

(defn pair? [coll]
  (cond
    (empty? coll) false
    (atom? coll) false
    (empty? (rest coll)) false
    (empty? (rest (rest coll))) true
    :else false))

(defn rcount [coll]
  (apply +  (flatten (pwalk-a (fn [a] 1) coll))))

(defn pair? [coll]
  (= 2 (rcount coll)))

(rcount [1 2 [3 4]])

(pair? [[1 [1]][1]])
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
