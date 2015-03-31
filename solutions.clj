(ns Clojure4.solution)

;; 19
(defn my-last [coll]
  (nth coll (dec (count coll))))


;; 20
(defn penultimate [coll]
  (if (<= (count coll) 2)
    (first coll)
    (nth coll (- (count coll) 2))))
;; or: #(second (reverse %))


;; 21
(defn my-nth [coll num]
  (loop [c coll idx 0]
    (if (= idx num)
      (first c)
      (recur (rest c) (inc idx)))))
;; or: #(get (vec %) %2)


;; 22
(defn my-count [coll]
  (loop [cnt 0 c coll]
    (if (empty? c)
      cnt
      (recur (inc cnt) (rest c)))))


;; 23
(defn my-reverse [coll]
  (loop [result () c coll]
    (if (empty? c)
      result
      (recur (conj result (first c)) (rest c)))))
;; or: (fn [coll] (reduce conj () coll))
;; or: (into ())


;; 26
(defn fibonacci [num]
  (letfn [(create-pairs [[a b]] [b (+ a b)])]
    (rest (map first (take (+ 1 num) (iterate create-pairs [0 1]))))))


;; 27
(defn palindrome? [x]
  (if (string? x)
    (= (apply str (reverse x)) (str x))
    (= (reverse x) x)))
;; or: (defn palindrome? [x]
;;       (= (seq x) (reverse x)))


;; 28
(defn my-flatten [coll]
  (let [[x & xs] coll]
    (cond
     (empty? coll) '()
     (coll? x)      (concat (my-flatten x) (my-flatten xs))
     :else          (cons x (my-flatten xs)))))


;; 29
(defn get-caps [input-string]
  (apply str (filter #(Character/isUpperCase %) input-string)))


;; 30
(defn compress [coll]
  (let [[x & xs] coll]
    (cond
     (empty? coll) []
     (= x (first xs)) (compress (drop 1 coll))
     :else            (concat [x] (compress (drop 1 coll))))))
;; (fn [x] (map first (partition-by identity x)))


;; 31
(defn pack [coll]
  (partition-by identity coll))


;; 32
(defn duplicate [coll]
  (interleave coll coll))


;; 33
(defn my-replicate [coll num]
  (if (= 1 num)
    coll
    (let [c (repeat num coll)]
      (apply interleave c))))


;; 34
(defn my-range [a b]
  (loop [x a result []]
    (if (= x b)
      result
      (recur (inc x) (conj result x)))))
;; or: (defn my-range* [a b]
;;       (take (- b a) (iterate inc a)))


;; 35
(defn my-max
  ([a] a)
  ([a b] (if (> a b) a b))
  ([a b & more] (reduce my-max (my-max a b) more)))


;; 39
(defn my-interleave [coll1 coll2]
  (let [csize (min (count coll1) (count coll2))]
    (loop [c1 coll1 c2 coll2 sz 0 result []]
      (if (= sz csize)
        result
        (recur (rest c1) (rest c2) (inc sz) (conj result (first c1) (first c2)))))))
;; :or (mapcat list)


;; 40
(defn my-interpose [x coll]
  (loop [c coll result []]
    (if (= 1 (count c))
      (concat result (vector (last c)))
      (recur (rest c) (concat result (vector (first c)) (vector x))))))
;; :or (fn [x coll] (drop 1 (interleave (repeat (count coll) x) coll)))


;; 41
(defn drop-nth [coll n]
  (letfn [(smart-drop [c]
            (if (= n (count c))
              (drop-last c)
              c))]
    (flatten (map smart-drop (partition-all n coll)))))


;; 42
(defn fact [n]
  (loop [num n result 1]
    (if (= num 1)
      result
      (recur (dec num) (* result num)))))
;; or: #(apply * (range (1 (inc %))))


;; 44
(defn rotate [n coll]
  (let [c (count coll)]
    (cond
     (neg? n) (rotate (+ c n) coll)
     (> n c)  (rotate (- n c) coll)
     :else    (concat (drop n coll) (take n coll)))))


;; 46
(defn flipping-out [f]
  (fn [arg2 arg1] (f arg1 arg2)))


;; 49
(defn my-split-at [n coll]
  (vector (take n coll) (drop n coll)))


;; 50
(defn split-by-type [coll]
  (vals (group-by type coll)))


;; 54
(defn my-partition [n coll]
  (when-let [s (seq coll)]
    (let [p (take n s)]
      (when (= (count p) n)
        (cons p (my-partition n (drop n s)))))))


;; 56
(defn my-distinct [coll]
  (loop [c coll result []]
    (if (empty? c)
      result
      (recur
       (rest c)
       (if (nil? (some #{(first c)} result)) (conj result (first c)) result)))))


;; 57
(defn foo [x]
  (when (> x 0)
    (conj (foo (dec x)) x)))


;; 61
(defn map-construct [coll1 coll2]
  (apply hash-map (interleave coll1 coll2)))
;; or #(into {} (map vector % %2))


;; 62
(defn my-iterate [f x]
  (lazy-seq (cons x (my-iterate f (f x)))))


;; 66
(defn gcd [a b]
  (let [divisor (min a b) dividend (max a b) remainder (rem dividend divisor)]
    (if (zero? remainder)
      divisor
      (gcd remainder divisor))))


;; 67
(defn multiples [num x]
  (vec
   (for [n (range 1 (inc (int (/ num x))))]
     (* n x))))

;; keep the non-divisible numbers
(defn divisible? [x num]
   (if (zero? (rem num x)) false true))

;; using the sieve of eratosthenes
;; find prime numbers upto num
(defn prime-numbers [num]
  (lazy-seq
   (letfn [(divisible? [x num] (if (zero? (rem num x)) false true))]
     (loop [num-range (range 2 (inc num)) n (first num-range) result []]
       (if (empty? num-range)
         result
         (recur
          (filter (partial divisible? n) num-range)
          (first (filter (partial divisible? n) num-range))
          (conj result n)))))))

;; using a fairly brute force technique, find the first x prime numbers
(defn primes [x]
  (letfn [(isDivisible? [numer denom] (if (zero? (rem numer denom)) true false))
          (isPrime? [a]
            (cond
             (even? a) false
             :else (empty? (filter (partial isDivisible? a) (range 3 a 2)))))]
    (cond
     (= x 1) [2]
     (= x 2) [2 3]
     :else (loop [counter 2 num 5 result [2 3]]
             (if (= counter x)
               result
               (recur
                (if (isPrime? num) (inc counter) counter)
                (+ 2 num)
                (if (isPrime? num) (conj result num) result)))))))


;; 74
(defn filter-perfect-square [coll]
  (let [perfect-square? (fn [num]
                          (let [num-sqrt (int (Math/sqrt num))]
                            (= num (* num-sqrt num-sqrt))))
        int-vec (map #(Integer/parseInt %) (clojure.string/split coll #","))]
    (clojure.string/join "," (map #(str %) (filter perfect-square? int-vec)))))


;; 75
(defn totient [num]
  (letfn [(gcd [a b]
            (let [divisor (min a b) dividend (max a b) remainder (rem dividend divisor)]
              (if (zero? remainder)
                divisor
                (gcd remainder divisor))))]
    (count (filter #(= 1 %) (map (partial gcd num) (range 1 (inc num)))))))


;; 80
(defn prime-numbers [num]
  (lazy-seq
   (letfn [(divisible? [x num] (if (zero? (rem num x)) false true))]
     (loop [num-range (range 2 (inc num)) n (first num-range) result []]
       (if (empty? num-range)
         result
         (recur
          (filter (partial divisible? n) num-range)
          (first (filter (partial divisible? n) num-range))
          (conj result n)))))))

(defn perfect-number [num]
  (let [prime-vec (prime-numbers num)
        prime-divisors (filter #(= 0 (rem num %)) prime-vec)]
    (= num (reduce + 1 prime-divisors))))


;; 81
(defn my-intersection [set1 set2]
  (loop [s1 set1 result #{}]
    (if (empty? s1)
      result
      (if (nil? (get set2 (first s1)))
        (recur (rest s1) result)
        (recur (rest s1) (conj result (first s1)))))))
;; or: #(set (filter % %2))
;; or:
;; (defn set-intersection [x y]
;;   (let [x-sub-y (clojure.set/difference x y)
;;         y-sub-x (clojure.set/difference y x)
;;         full-diff (clojure.set/union x-sub-y y-sub-x)
;;         full-set (clojure.set/union x y)]
;;     (clojure.set/difference full-set full-diff)))


;; 83
(defn half-truth? [& more]
  (cond
   (every? true? more) false
   (every? false? more) false
   :else true))
;; or: (defn half-truth? [& more]
;;       (= 2 (count (set more))))


;; 86
;; break a number into digits
(defn digits [num]
  (loop [result '() n num]
    (if (zero? (quot n 10))
      (conj result n)
      (recur (conj result (rem n 10)) (quot n 10)))))

(defn sum-of-squares [num]
  (let [num-list (digits num)]
    (reduce + (map #(* % %) num-list))))

;;(defn happy-number [num]
;;  (iterate sum-of-squares num))

(defn happy-number [num]
  (loop [c [] n num]
    (cond
     (= 1 n) true
     (not (nil? (some #{n} c))) false
     :else (recur (conj c n) (sum-of-squares n)))))


;; 88
(fn symmetric-diff [s1 s2]
  (let [inter (clojure.set/intersection s1 s2)
        just-s1 (clojure.set/difference s1 inter)
        just-s2 (clojure.set/difference s2 inter)]
    (clojure.set/union just-s1)))


;; 90
(defn cartesian-prod [s1 s2]
  (set (for [a s1 b s2] [a b])))


;; 97
(defn pascal-row [row]
  (letfn [(fact [n] (reduce * (range 1 (inc n))))
          (elem-val [r e] (/ (fact r) (* (fact e) (fact (- r e)))))]
    (map (partial elem-val (dec row)) (range row))))


;; 99
(defn prod-digits [a b]
  (let [prod (str (* a b))]
    (map #(Character/getNumericValue %) prod)))


;; 100
(defn lcm [& nums]
  (letfn [(gcd [a b]
            (let [divisor (min a b) dividend (max a b) remainder (rem dividend divisor)]
              (if (zero? remainder)
                divisor
                (gcd remainder divisor))))]
    (/ (reduce * nums) (reduce gcd nums))))


;; 116
(defn prime-sandwich [num]
  (letfn [(isDivisible? [numer denom] (if (zero? (rem numer denom)) true false))
          (isPrime? [a]
            (cond
             (even? a) false
             :else (empty? (filter (partial isDivisible? a) (range 3 a 2)))))
          (nxt-prime [a]
            (if (isPrime? (+ 2 a)) (+ 2 a) (nxt-prime (+ 2 a))))
          (prv-prime [a]
            (if (isPrime? (- a 2)) (- a 2) (prv-prime (- a 2))))
          (average [a b] (/ (+ a b) 2))]
    (cond
     (= 0 num) false
     (= 1 num) false
     (= 2 num) false
     (= 3 num) false
     (isPrime? num) (= num (average (nxt-prime num) (prv-prime num)))
     :else false)))


;; 118
(defn my-map [func coll]
  (if (seq coll)
    (lazy-seq
     (cons (func (first coll)) (my-map func (rest coll))))
    nil))
;; this version is too slow ...
;; (defn my-map [func coll]
;;     (lazy-seq
;;      (loop [result [] c coll]
;;        (if (empty? c)
;;          result
;;          (recur (conj result (func (first c))) (rest c))))))


;; 120
(defn sum-squares [coll]
  (letfn [(digits  [num]
            (loop [result '() n num]
              (if (zero? (quot n 10))
                (conj result n)
                (recur (conj result (rem n 10)) (quot n 10)))))
          (sos [num]
            (let [num-list (digits num)]
              (reduce + (map #(* % %) num-list))))]
    (count (filter #(< % (sos %)) coll))))


;; 122
(defn read-binary [str-num]
  (let [binary-vec (reverse (map #(Character/getNumericValue %) str-num))]
    (int (reduce + (map-indexed #(* (Math/pow 2 %1) %2) binary-vec)))))
;; or: #(read-string (str "2r" %))


;; 134
(defn nil-key? [some-key some-map]
  (if (contains? some-map some-key)
    (nil? (some-key some-map))
    false))


;; 135
(defn infix-calculator [x & xs]
  (loop [result x a xs]
    (if (= 2 (count a))
      ((first a) result (second a))
      (recur ((first a) result (second a)) (rest (rest a))))))
;; or
;; (defn infix-calculator
;;   ([a] a)
;;   ([a b c & m]
;;      (apply infix-calculator (b a c) m)))


;; 137
(defn digits-and-bases [num base]
  (loop [result '() n num]
    (if (zero? (quot n base))
      (conj result n)
      (recur (conj result (rem n base)) (quot n base)))))


;; 143
(defn dot-product [coll1 coll2]
  (reduce + (map * coll1 coll2)))


;; 147
(defn pascal-trapezoid [coll]
  (letfn [(next-pascal-row [coll]
            (loop [result (vector (first coll)) c coll]
              (if (= 1 (count c))
                (conj result (last coll))
                (recur (conj result (+ (first c) (second c))) (rest c)))))]
    (iterate next-pascal-row coll)))


;; 148 TOO SLOW for large numbers
(defn big-divide [num div1 div2]
  (letfn [(divisible? [n] (or (= (rem n div1) 0) (= (rem n div2) 0)))]
    (reduce + (filter divisible? (range 1 num)))))

(defn factors [num]
  (let [n (int (Math/sqrt num))
        divides? (fn [x] (if (zero? (rem num x)) true false))
        first-half (filter divides? (range 1 (inc n)))
        second-half (map #(/ num %) (reverse first-half))]
    (distinct (concat first-half (drop-last second-half)))))

(defn even-divisors [num x]
  (let [check (if (even? num) (/ num 2) (inc (/ num 2)))
