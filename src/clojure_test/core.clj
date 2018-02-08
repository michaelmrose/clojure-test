(ns clojure-test.core
  (:gen-class)
  (:refer-clojure)
  (:import
   [java.util Random])
  (:require
   [cats.core :as m :refer [alet fapply mappend mlet mplus] :rename {mplus ||}]
   [cats.builtin]
   [clojure.java.io :refer [reader]]
   [cats.monad
    [either :refer :all]
    [exception :as exc]
    [maybe :as maybe :refer [just nothing]]]
   [clojure.core.match :refer [match]]
   [clojure.math.numeric-tower :as math]
   [com.rpl.specter :as specter :refer :all]
   [puget.printer :as puget]
   [special.core :refer [condition manage]]
   [swiss.arrows :refer :all]
   [thoughts.core :as wtf :refer [answer]]
   ;; [test.carly.core :as carly :refer [defop]]
   [clojure.string :as string]
   [clojure.spec.alpha :as s]
   [clojure.spec.test.alpha :as stest]
   [expound.alpha :as expound]
   [cuerdas.core :as str]
   [cemerick.pomegranate :refer [add-dependencies]]
   ))

(s/check-asserts true)
;; (stest/instrument (ns-publics))
(def != (complement =))

(defn deps [dep]
  (add-dependencies :coordinates dep
                    :repositories (merge cemerick.pomegranate.aether/maven-central
                                         {"clojars" "https://clojars.org/repo"})))


(deps '[[org.clojure/core.match "0.3.0-alpha5"]])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Fuck you very very much, World!")
  (println "Hello, World!"))


(defn last?
  "Returns true if coll is empty or contains only a singular item"
  [coll]
  (not (next coll)))

(last? [1])

(last [1 2 3])

(defn car [v]
  (cond
    (string? v) (str (first v))
    (coll? v) (first v)))
(defn cdr [v]
  (cond
    (string? v) (string/join (rest v))
    (coll? v) (rest v)))

(puget/pprint 7)
(math/abs 7)

(defn beg [col]
  (take (- (count col) 1) col))

(defn listify [t]
  (if (coll? t) (into [] t) [t]))

(defn sum [values]
  (apply + values))

(defn find-thing [needle haystack]
  (keep-indexed #(when (= %2 needle) %1) haystack))

(defn map-nn [f col] (remove nil? (map f col)))

(defn within
  "Returns true if NUMBER is within 
  LOW inclusive and HIGH inclusive"
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
(defn contains-item? [t l]
  (if (not-empty (find-thing t l)) true false))

(contains-item? "fuck" [1 2 3 4 "fuck"])
(find-thing 4 [1 2 3 4 "fuck"])

;; (defn slice [coll beg end]
;;   (let [end (if (number? end) end (count coll))]
;;     (-<> (drop (dec beg) coll)
;;          (take (- end (dec beg)) <>))))

(defn slice
  ([coll beg]
   (slice coll beg (count coll)))
  ([coll beg end]
   (-<> (drop (dec beg) coll)
        (take (- end (dec beg)) <>))))

(slice [:1 :2 :3 :4] 2 3)

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

(defn i [mv]
  (m/bind mv identity))

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
(i (exc/try-or-else (+ 1 nil) 42))
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
              :investment-types [{:type :silver
                                  :markets [{:name "Japan"
                                             :value 7000}

                                            {:name "America"
                                             :value 1000
                                             :bullshit 7}]}
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

(transform [:outer :max] (fn [n] (+ 9 n)) {:outer {:max 30 :min 10}})
(select [:clients ALL :investment-types ALL :type] colin)
(select [:client ALL :investment-types ALL :type] colin)
;; (select [(must :client) ALL :investment-types ALL :type] colin)
(transform [:clients ALL :investment-types ALL :type] (fn [n] :bullshit) colin)
(transform [(filterer #(< % 3)) LAST]
           inc
           [2 1 3 6 9 4 8])
;; (extract colin :investment-types)

(filter identity [nil 7 nil])

;; (defn extract
;;   ([key coll]
;;    (if (coll? key)
;;      (map #(extract % coll) key)
;;      (->> (select (walker key) coll)
;;           (map key))))
;;   ([key coll f]
;;    (reduce f (extract key coll))))
(defn flat? [coll]
  (every? atom coll))

(defn rempty? [coll]
  (or (and (empty? coll)
           (flat? coll))
      (every? empty coll)))

(defn extract-value [coll key]
  (into [] (map key (select (walker key) coll))))

(defn extract-combine [coll & keys]
  (flatten (map #(extract-value coll %) keys)))

;; (defn extract [coll targets]
;;   (if (= (count targets) 1)
;;     (extract-value coll (first targets))
;;     (extract (extract-value coll (first targets)) (rest targets))))

;; (defn extract [coll targets]
;;   (let [t (listify targets)]
;;     (if (= (count t) 1)
;;       (extract-value coll (first t))
;;       (extract (extract-value coll (first t)) (rest t)))))

;; (defn extract [coll targets]
;;   (let [t (listify targets)]
;;     (if (empty? t) coll
;;       (extract (extract-value coll (first t)) (rest t)))))
(defn extract [coll & targets]
  (cond
    (empty? coll) nil
    (rempty? targets) coll
    :else (recur (extract-value coll (first targets)) (rest targets))))
(extract colin :markets :name)
(extract colin :markets)
(extract colin :type)
(extract colin :markets)

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

(def nil-chainer {:step-runner call-unless-nil
                  :inner-wrapper identity})

(def list-chainer {:step-runner mapcat
                   :inner-wrapper list})

(defn wrap-last [wrapper steps]
  (let [last (last steps)
        wrapped (comp wrapper last)]
    (replace {last wrapped} steps)))

(defn chain
  [{:keys [step-runner inner-wrapper]} value steps]
  (reduce (flip step-runner)
          value
          (wrap-last inner-wrapper steps)))

(chain nil-chainer colleen [:pet :mother :owner :name])
(chain nil-chainer richard [:pet :mother :owner :name])
(chain list-chainer (colin :clients) [:investment-types :markets :value])
(chain list-chainer (richard :clients) [:investment-types :markets :value])
(m/fmap inc (just 7))
(m/fmap inc [1 2 3])
(m/fmap inc (nothing))

(defn make-greeter
  [^String lang]
  (condp = lang
    "es" (fn [name] (str "Hola " name))
    "en" (fn [name] (str "Hello " name))
    nil))

(defn make-greeter
  [^String lang]
  (condp = lang
    "es" (just (fn [name] (str "Hola " name)))
    "en" (just (fn [name] (str "Hello " name)))
    (nothing)))

(defn ucase [s]
  (.toUpperCase s))

(defn dcase [s]
  (.toLowerCase s))

(m/fmap ucase (fapply (make-greeter "en") (just "Alex")))
(m/fmap ucase (fapply (make-greeter "ex") (just "Alex")))
(i (fapply (make-greeter "en") (just "Alex")))
(i (mappend (just [4 5 6]) (just [1 2 3]) (nothing)))
(let [mgr (fapply (make-greeter "en") (just "Alex"))
      upper (m/fmap ucase mgr)]
  upper)

(mlet [name (just "Alex")
       bs (just 7)
       morebs (just (inc bs))]
      (m/return morebs))

(mlet [a (maybe/just 1)
       b (maybe/just (inc a))]
      (m/return (* b 2)))

(-<> (fapply (make-greeter "en") (just "Alex"))
     (m/fmap ucase <>)
     (m/fmap dcase <>)
     (i))
;; (-<> ((make-greeter "en") "Frank")
;;      (.toUpperCase)
;;      )
(i (m/fmap ucase (alet [name (just "bob")
                        greeter (make-greeter "en")]
                       (greeter name))))
;; (-<> (make-greeter "en")
;;      (apply <> ["Bob"])
;;      (.toUpperCase <>))

(i (fapply (make-greeter "es") (just "Alex")))
;; => #<Just "Hola Alex">
(fapply (make-greeter "es") (just "Alex"))

(fapply (make-greeter "en") (just "Alex"))
;; => #<Just "Hello Alex">

(fapply (make-greeter "it") (just "Alex"))
;; => #<Nothing>
(left "fuck")
(right "fuck")
(m/mplus (nothing) (just 7))
(|| (nothing) (just 7) (just 8))

(+ 7 (+ 7 60))

(beg [1 2 3])

(-<> (+ 7 7)
     (+ 2 <>))

(def vs [:a nil :c])

(def ps [:1 :2 :3 :4 :5 :6 :7])

(defn nn [x]
  (filter #(not-any? nil? %) x))

(nn (for [v vs
          p ps]
      [v p]))

(-<> (for [v vs
           p ps]
       [v p])
     nn)

(defn fa [x]
  (+ x 1))
(defn fb [x]
  (+ x 1))
(defn fc [x]
  (+ x 1))

;; (def fb nil)
;; (def fb (nothing))
(-> (fa 7)
    fb
    fc)
(filter #(not-any? nil? %) (for [v vs
                                 p ps]
                             [v p]))
;; + 7 7 | + 2 <>
;; + 7 7 -> + 2 <>
;; + 7 7 => + 2 <>
;; (+ 7 7) | (+ 2 <>)

(ucase "fuck")
((comp #(+ 1 %) #(+ 2 %) #(+ 3 %)) 7)
(defn rstr [s]
  (string/replace s #"^bob" "fuck"))
(rstr "bob")
((comp ucase rstr) "bob")
(ucase (rstr "bob"))
(-<> "   bob afasdf adfa     a  "
     (string/trim <>)
     (#(string/replace % #"^bob" "fuck") <>)
     (#(string/replace % #"\s+" " "))
     ucase)
((comp ucase string/trim #(string/replace % #"^bob" "fuck")) "bob")
((partial + 7) 7)




(defn validate [spec value]
  (if-not (s/valid? spec value)
    (let [explanation (s/explain-data spec value)
          explanation-string (s/explain-str spec value)
          issue (last (:clojure.spec/problems explanation))
          pred (:pred issue)
          val (:val issue)
          via (:via issue)
          msg (str "value: " val " failed predicate " pred " in " via "\n\n\n")]
      (do (puget/cprint (str "problem found while validating " via))
          ;; (puget/cprint value)
          ;; (println msg)
          (puget/cprint explanation)
          (throw (ex-info (str msg ": " explanation-string) explanation))))
    value))
;; (validate ::nandks [[1] [:fuck]])
;; (validate ::nandks [[] [:fuck]])
;; (defn validate [spec value]
;;   (if-not (s/valid? spec value)
;;     (throw (ex-info (str "SPEC VIOLATION: "
;;                          s/explain-str spec value)
;;                     (s/explain-data spec value)))))


(s/def ::nandks (s/cat :nums (s/spec (s/+ number?))
                       :keys (s/spec (s/+ keyword?))))
(if (= 1 1) false true)
(dcase (ucase "fuck"))

(s/valid? ::nandks [[1] [:a :b]])
(s/valid? ::nandks [["fuck"] [:a :b]])
(s/valid? ::nandks [[] [:a :b]])
((frequencies [1 1 2 2 2 3]) 3)
;; (validate ::nandks [["fuck"][:a :b]])
;; (defn- handle-node
;;   [children]
;;   (fn [child index]
;;              (let [subtree (render-tree child)
;;                    last? (= index (dec (count children)))
;;                    prefix-first (if last? L-branch T-branch)
;;                    prefix-rest  (if last? SPACER I-branch)]
;;                (cons (str prefix-first (first subtree))
;;                      (map #(str prefix-rest %) (next subtree))))))

;; (defn render-tree [{:keys [name children]}]
;;   (cons
;;    name
;;    (mapcat (handle-node children)
;;            children
;;            (range))))

;; (let [[_ _ z :as vals] [1 2 3]]
;;   vals)
;; (stest/check `fir)
;; (stest/instrument `fir)
;; (s/def ::coll #(coll? %))
(s/def ::number #(number? %))
#_(defn foo
    [n]
    (validate  ::number n)
    (let [res (cond (> n 40) (+ n 20)
                    (> n 20) (- (fir n) 20)
                    :else 0)]
      (validate ::number res)))
#_(defn foo
    [n]
    (validate ::number n)
    (validate ::number (cond (> n 40) (+ n 20)
                             (> n 20) (- (first n) 20)
                             :else "fuck")))

(defn foo
  [n]
  {:pre [(s/valid? ::number n)]
   :post [(s/valid? ::number %)]}

  (cond (> n 40) (+ n 20)
        (> n 20) (- (first n) 20)
        :else "fc"))
;; (foo 7)

(loop [result [] x 5]
  (if (< x -3)
    result
    (recur (conj result x) (dec x))))

(take 3 (iterate dec 5))
(defn constrained-fn [f x]
  {:pre  [(pos? x)]
   :post [(= % (* 2 x))]}
  (f x))

(constrained-fn #(* % 2) 5)

#_(defn indexOfAny
    ([collOfLetters targetString]
     (indexOfAny collOfLetters targetString 0))
    ([collOfLetters targetString idx]
     (cond
       (empty? targetString) -1
       (some #(= true %) (map #(= (car targetString) %) collOfLetters)) idx
       :else (recur collOfLetters (cdr targetString) (inc idx)))))

(defn anyEqualTo [v vs]
  "Return true if any in vs are equal to v"
  (or (some #(= true %) (map #(= v %) vs))) false)

(anyEqualTo 9 [7 2 3])

#_(defn indexOfAny [collOfLetters targetString]
    (loop [collOfLetters collOfLetters targetString targetString idx 0]
      (cond
        (empty? targetString) -1
        (some #(= true %) (map #(= (car targetString) %) collOfLetters)) idx
        :else (recur collOfLetters (cdr targetString) (inc idx)))))

#_(defn indexOfAny [collOfLetters targetString]
    (loop [collOfLetters collOfLetters targetString targetString idx 0]
      (cond
        (empty? targetString) -1
        (anyEqualTo (car targetString) collOfLetters) idx
        :else (recur collOfLetters (cdr targetString) (inc idx)))))

(defn indexOfAny [collOfLetters targetString]
  (loop [collOfLetters collOfLetters targetString targetString idx 0]
    (cond (empty? targetString) -1
          (contains-item? (car targetString) collOfLetters) idx
          :else (recur collOfLetters (cdr targetString) (inc idx)))))

(indexOfAny ["r" "q"] "zzabyycdxx")
(indexOfAny ["r" "y"] "zzabyycdxx")

(defn indexed [coll] (map-indexed vector coll))

(defn index-filter [letters s]
  (when pred
    (for [[idx letter] (indexed s) :when (contains-item? letter letters)] idx)))

(defn index-filter [pred coll]
  (when pred
    (for [[idx elt] (indexed coll) :when (pred elt)] idx)))

(defn indexOfAnyfn [letters s]
  (first (index-filter letters s)))

(indexOfAny ["d"] "abcdbbb")
(indexOfAnyfn #{\d \b} "abcdbbb")
;;----------------------------------------------------
(defn random-flip []
  (if (= (rand-int 2) 0)
    :h :t))

(def reallylongflips (take 7777 (repeatedly random-flip)))
(def count-if (comp count filter))

(defn count-runs [n pred coll]
  (count-if #(every? pred %) (partition n 1 coll)))

;; (defn count-runs [n pred coll]
;;   (count-if #(pred %) (partition n 1 coll)))

;; (defn count-heads-pairs [coll]
;;   (count-runs 2 #(= [:h :h] %) coll))

;; (defn count-tails-pairs [coll]
;;   (count-runs 2 #(= [:t :t] %) coll))

;; (defn count-pairs [coll]
;;   (count-runs 2 #(= (first %) (second %)) coll))

#_(defn count-heads-pairs [coll] 
    (count-if
     (fn [x] (= [:h :h] x))
     (partition 2 1 coll)))

#_(defn count-pairs [coll] 
    (count-if
     (fn [x] (=  (first x) (second x)))
     (partition 2 1 coll)))
(def count-heads-pairs (partial count-runs 2 #(= % :h)))
(def count-tails-pairs (partial count-runs 2 #(= % :t)))
(def count-pairs
  (comp #(apply + %) (juxt count-heads-pairs count-tails-pairs)))

(time (count-pairs reallylongflips))
(time (count-heads-pairs reallylongflips))
(time (count-tails-pairs reallylongflips))
;;----------------------------------------------------

(defn count-pairs [coll]
  (count (filter #(= (first %) (second %)) (partition 2 1 coll ))))

#_(defn by-pairs [coll]
    (let [take-pair (fn [c]
                      (when (next c) (take 2 c)))]
      (lazy-seq
       (when-let [pair (seq (take-pair coll))]
         (cons pair (by-pairs (rest coll)))))))

(defn preds []
  (into []
        (comp (map ns-publics)
              (mapcat vals)
              (filter #(string/ends-with? % "?"))
              (map #(str (.-sym %))))
        (all-ns)))

(defn non-blank? [line] (not (string/blank? line)))

(defn non-blank-lines-eduction [reader]
  (eduction (filter non-blank?) (line-seq reader)))

(s/def ::validcolors #{:red :green :blue})
(s/def ::validnumbers (s/int-in 1 10))
(s/def ::even-coll (s/coll-of even?))
(s/def ::n-coll (s/coll-of number?))
(s/def ::test (s/and ::even-coll ::n-coll))
(s/valid? ::test [2 5 8])
(s/def ::my-set (s/coll-of int? :kind set? :min-count 2 :max-count 3))
(s/explain-str ::my-set #{1 2 3})
(s/explain-str ::my-set (hash-set 1 2 3 3 2))

#_(defn count-heads-pairs [coll]
    (loop [cnt 0 coll coll]
      (if (empty? coll)
        cnt
        (recur (if (= :h (first coll) (second coll))
                 (inc cnt)
                 cnt)
               (rest coll)))))

;; (count-heads-pairs [:h :t :h :h :t :h :h :h :t :h])

;; (take 7 (repeatedly random-flip))






#_(defn by-pairs [coll]
    (lazy-seq
     (if (last? coll) nil
         (cons (take 2 coll)
               (by-pairs (rest coll))))))

#_(by-pairs reallylongflips)
#_(count-heads-pairs reallylongflips)

#_(defn by-pairs [coll]
    (cond
      (< (count coll) 2) '()
      :else (cons (take 2 coll) (by-pairs (rest coll)))))

#_(defn by-pairs [coll]
    (loop [coll coll res []]
      (cond
        (empty? coll) res
        :else (recur (rest coll) (cons (take 2 coll) res)))))

;; (by-pairs [:h :h :t])
;; (by-pairs '(:h :h :t :h))

#_(defn countp [pred coll]
    (count (filter pred coll)))


#_(defn count-heads-pairs [coll]
    (countp #(= [:h :h] %) (by-pairs coll)))

;; (defn count-heads-pairs [coll]
;;   (count (filter #(= [:h :h] % ) (by-pairs coll))))

;; (defn count-pairs [coll]
;;   (count (filter #(= (first %) (second %)) (by-pairs coll))))



;; (count-pairs reallylongflips)
;; (count-tails-pairs reallylongflips)
;; (count-heads-pairs reallylongflips)


;; (defn count-heads-pairs [coll]
;;   (count (filter (fn [pair] (every? #(= :h %) pair))
;;                  (by-pairs coll))))
;; (def ridiculous (string/join (repeat 2333333 "fuck")))
;; (time (indexOfAny [\f] ridiculous)) ;; (time (indexOfAny #{\f} ridiculous))
;; (time (indexOfAny ["f"] ridiculous))
;; (time (count-heads-pairs reallylongflips))
;; (count-heads-pairs [:h :t :h :h :t :h :h :h :t :h])
;; (count-heads-pairs [:t :t :h :h :h :h :t])
;; (count-pairs       [:t :t :h :h :h :h :t])
;; (count-pairs [:h :h :h])
;; (count-heads-pairs [:h :h :h])
;; (declare my-odd? my-even?)
;; (defn my-odd? [n]
;;   (if (= n 0)
;;     false
;;     #(my-even? (dec n))))

;; (defn my-even? [n]
;;   "mutually recursive even?"
;;   (if (= n 0)
;;     true
;;     #(my-odd? (dec n))))

;; (trampoline (my-odd? 7))


(s/def ::steps (s/coll-of string?))
(s/def ::result string?)
(s/def ::ingredient (s/cat :amount number?
                           :unit keyword?
                           :name string?))
(s/def ::ingredients (s/+ ::ingredient))

(s/def ::recipe (s/keys :req [::ingredients ::steps ::result]))

;; (defn ingredient-is-scaled? [recipe item scale]
;;   (= (:amount item) (* scale (:amount (get-ingredient recipe (:name item))))))
(defn get-ingredient [recipe item]
  (first (filter #(= item (:name %))
                 (s/conform ::ingredients (::ingredients recipe)))))
(defn ingredient-is-scaled? [recipe item scale]
  (= (:amount item)
     (-<> (:name item)
          (get-ingredient recipe <>)
          (:amount <>)
          (* scale <>))))


;; (s/def ::doubleingredient ingredient-is-double?)

(def badrecipe {::ingredients [1 :kg "apples"]
                ::steps [99]
                ::result "diahrea"})

(def badrecipe2 {::ingredients [1 :kg]
                 ::steps ["eat apples"]
                 ::result "diahrea"})

(def goodrecipe
  {::ingredients
   [1 :kg "aubergines"
    20 :ml "soysauce"]
   ::steps ["fry the aubergines"
            "add the soy sauce"]
   ::result "fried aubergines"})

(s/fdef scale-ingredient
        :args (s/cat :ingredient ::ingredient :factor number?)
        :ret ::ingredient)

(defn scale-ingredient [ingredient factor]
  (update ingredient :amount * factor))

(scale-ingredient (first (s/conform ::ingredients (::ingredients goodrecipe))) 2)



(get-ingredient goodrecipe "soysauce")


(s/conform ::ingredients (::ingredients goodrecipe))
(defn cook! [recipe]
  (validate ::recipe recipe)
  (str (::result recipe)))

(mapcat #(inc %) [1 2 3])
(mapcat #(string/split % #"\d") ["aa1bb" "cc1DD"])

(mapcat reverse [[3 2 1 0] [6 5 4] [9 8 7]])
(apply concat (map reverse [[1 2 3] [4 5 6]]))

(ingredient-is-scaled? goodrecipe (scale-ingredient (get-ingredient goodrecipe "soysauce") 2) 2)
(def current-track (ref "Mars, the Bringer of War"))
(def current-composer (ref "Holst"))
(deref current-track)
@current-track
(dosync
 (ref-set current-track "Venus, the Bringer of Peace"))

(dosync
 (ref-set current-track "Credo")
 (ref-set current-composer "Byrd"))

@current-composer

(defrecord Message [sender text])

(Message. "Aaron" "Hello")


(def mynumber (ref 1 :validator odd?))

(defn incmynumber [n]
  (dosync (alter mynumber + n)))

(defn decmynumber [n]
  (dosync (commute mynumber - n)))

(incmynumber 6)
(decmynumber 2)


(defn valid-message? [msg]
  (and (:sender msg)(:text msg) (= (type msg) clojure_test.core.Message)))

(def validate-message-list  #(every? valid-message? %))


(def messages (ref () :validator validate-message-list))

(defn add-message [msg]
  (dosync (ref-set messages (cons msg @messages))))


(defn add-message [msg]
  (dosync (commute messages conj msg)))

(add-message (->Message "Aaron" "Fuck"))
(add-message (->Message "Aaron" "fuckity fuck"))
(add-message (->Message "God" "die bitch"))

(def backup-agent (agent "/home/michael/proj/clojure/clojure-test/output/messages-backup.clj"))

(do (print "----------------------\n")
    (puget/cprint @messages)
    (print "----------------------\n"))
(do (print "----------------------\n")
    (puget/cprint (slurp @backup-agent))
    (print "----------------------\n"))



(defn add-message-with-backup [msg]
  (dosync
   (let [snapshot (commute messages conj msg)]
     (send-off backup-agent (fn [filename]
                              (spit filename snapshot)
                              filename)))))

;;-----------------
(defn add-message-with-backup [msg]
  (dosync 
   (let [snapshot (commute messages conj msg)]
     (send-off backup-agent (fn [filename]
                              (spit filename snapshot)
                              filename))
     snapshot)))
;;-----------------
(add-message-with-backup (->Message "John" "Message Seven"))
;; (restart-agent messages ())
(defrecord Person [fname lname address])

(defrecord Address [street city state zip])

(def stu (Person. "Stu" "Halloway"
                  (Address. "200 N Mangum"
                            "Durham"
                            "NC"
                            27701)))

(:lname stu)
(:fname stu)

(-> stu :address :city)

(assoc stu :fname "Stuart")
(def blarg (atom 7))
(reset! blarg 9)
(deref blarg)

(def current-track (atom {:title "Mars" :composer "Holst"}))
(swap! current-track assoc :title "Mars")
(swap! current-track assoc :title "Venus")
(reset! current-track {:title "Mars" :composer "Holst"})

@current-track


(def current-title (ref "Mars"))
(def current-composer (ref "Holst"))
(dosync (ref-set current-title "gahhh")
        (ref-set current-composer "wtf"))
(dosync (ref-set current-title "Venus")
        (ref-set current-composer "holst"))
(dosync (alter current-composer string/capitalize))
@current-composer
@current-title

(defn handler [agent err]
  (println "ERR!" (.getMessage err)))

(def counter (agent 0 :error-handler handler))
(def counter2 (agent 0 :validator number? :error-handler handler))

(send counter2 #("boo"))
(agent-error counter)
(send counter2 inc)
;; (restart-agent counter 0)
;; (deref counter2)
;; (defn slow-increase [a]
;;   (do (Thread/sleep 3000)
;;       (send-off a inc)))

;; (do (slow-increase counter)
;;     (await-for 2000 counter)
;;     (deref counter))
;; (set! s/*explain-out* expound/printer)

;; (s/def :example.place/city string?)
;; (s/def :example.place/state string?)
;; (s/fdef pr-loc :args (s/cat :city :example.place/city
;;                             :state :example.place/state))
;; (defn pr-loc [city state]
;;   (str city ", " state))

;; (stest/instrument `pr-loc)
;; (pr-loc "denver" :CO)

;; ;; You can use `explain` without converting to expound
;; (s/explain :example.place/city 123)
;; (print "-----------------------------------------------------------------")

;; (def inc-and-filter (comp (map inc) (filter odd?)))
;; (def special+ (inc-and-filter +))
;; (special+ 1 1)

;; (transduce (comp (map inc)))
;; (defn unless [expr form]
;;   (if (not expr)
;;     form))

(defmacro unless [expr form]
  (list 'if expr nil form))

(defmacro unless [expr & forms]
  (list 'if expr nil (cons 'do forms)))

(unless (= 1 1) (println "fuck"))
(unless (= 1 2) (println "fuck")(println "me"))

(defmacro chain
  ([x form] (list '. x form))
  ([x form & more] (concat (list 'chain (list '. x form)) more)))

(defmacro chain
  ([x form       ] `(. ~x ~form))
  ([x form & more] `(~concat (~list chain (~list . ~x ~form)) ~more)))

(defmacro chain
  ([x form       ] `(. ~x ~form))
  ([x form & more] `(chain (. ~x ~form) ~@more)))

(macroexpand '(chain arm getHand getFinger))

(let [start (System/nanoTime)
      result (str "a" "b")]
  {:result result :elapsed (- (System/nanoTime) start)})

(defmacro bench [expr]
  `(let [start# (System/nanoTime)
         result# ~expr]
     {:result result# :elapsed (- (System/nanoTime) start#)}))

(bench (str "a" "b"))
(declare fuck this shit)
(defmacro declare
  [& names] `(do ~@(map #(list 'def %) names)))

(defn say-hi []
  (print (str "Hello from thread" (.getName (Thread/currentThread)) "\n")))

(defn say-hi []
  (-<> (Thread/currentThread)
       (.getName <>)
       (str "Hello from thread" <> "\n")
       (print <>)))
(dotimes [_ 3]
  (.start (Thread. say-hi)) )

(. (Thread/currentThread) getName)

(.getName (Thread/currentThread))

(defmulti my-print class)

(defmethod my-print clojure.lang.IPersistentVector [v]
  (.write *out* "[")
  (.write *out* (str/join " " v))
  (.write *out* "]"))

(defmethod my-print String [s]
  (.write *out* s))

(defmethod my-print nil [_]
  (.write *out* "nil"))

(defmethod my-print :default [_]
  (.write *out* (.toString _)))

(defmethod my-print Person [p]
  (.write *out* (str "name: " (:fname p) " " (:lname p) "\n addr: " (:address p) )))

(defmethod my-print java.util.Collection [c]
  (.write *out* "(")
  (.write *out* (str/join " " c))
  (.write *out* ")"))

(prefer-method my-print java.util.Collection clojure.lang.IPersistentVector)
(def test-savings  {:id 1 :tag :acc/savings  :balance 10000M})
(def test-checking {:id 2 :tag :acc/checking :balance 250M})

(defmulti interest-rate :tag)

(defmethod interest-rate :acc/checking [_] 0M)

(defmethod interest-rate :acc/savings [_] 0.05M)

(interest-rate test-checking)

(defmulti account-level :tag)
(defmethod account-level :acc/checking [acct]
  (if (>= (:balance acct) 5000) :acc/premium :acc/basic))

(defmethod account-level :acc/savings [acct]
  (if (>= (:balance acct) 1000) :acc/premium :acc/basic))

(account-level test-savings)

(:tag test-savings)
(:tag test-checking)


(defn tag-and-type [acct] [(:tag acct) (account-level acct)])
(tag-and-type test-checking)

(derive :acc/savings :acc/account)
(derive :acc/checking :acc/account)
(defmulti service-charge tag-and-type)
(defmethod service-charge [:acc/checking :acc/basic] [_] 25)
(defmethod service-charge [:acc/savings :acc/basic] [_] 10)
(defmethod service-charge [:acc/premium :acc/account] [_] 0)
;; (defmethod service-charge [:acc/savings :acc/premium] [_] 0)

(defn pm-service-charge [acct]
  (match [(:tag acct) (account-level acct)]
         [:acc/checking :acc/basic] 25
         [:acc/savings :acc/basic] 10
         [_ :acc/premium] 0))

(pm-service-charge test-checking)
(pm-service-charge test-savings)

(defn useless-multi-line-fn []
  "one"
  "two"
  "four")
