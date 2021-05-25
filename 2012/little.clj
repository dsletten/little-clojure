;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               little.clj
;;;;
;;;;   Started:            Tue Apr 24 23:15:24 2012
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes:
;;;;
;;;;

(ns little
  (:use clojure.contrib.test-is
        [clojure.contrib.pprint :only (cl-format)]))

;;;
;;;    Ch. 1
;;;    
(defn atom? [obj]
  (not (coll? obj)))

;;;
;;;    Ch. 2
;;;    
(defn lat? [l]
  (cond (empty? l) true
        (atom? (first l)) (lat? (rest l))
        :else false))

(deftest test-lat?
  (is (lat? '(jack sprat could eat no chicken fat)))
  (is (not (lat? '((jack) sprat could eat no chicken fat))))
  (is (not (lat? '(jack (sprat could) eat no chicken fat))))
  (is (lat? '())))

(defn lat-1? [l]
  (every? atom? l))

(deftest test-lat-1?
  (is (lat-1? '(jack sprat could eat no chicken fat)))
  (is (not (lat-1? '((jack) sprat could eat no chicken fat))))
  (is (not (lat-1? '(jack (sprat could) eat no chicken fat))))
  (is (lat-1? '())))

(defn member [obj l]
  (cond (empty? l) false
        (= (first l) obj) true
        :else (member obj (rest l))))

(deftest test-member
  (is (member 'tea '(coffee tea or milk)))
  (is (member 'meat '(mashed potatoes and meat gravy)))
  (is (not (member 'poached '(fried eggs and scrambled eggs))))
  (is (not (member 'liver '(bagels and lox)))) )

;;;
;;;    Ch. 3
;;;    
(defn rember [a lat]
  (cond (empty? lat) '()
        (= a (first lat)) (rest lat)
        :else (cons (first lat) (rember a (rest lat)))) )

(defn rember [a lat]
  (loop [l lat
         result '()]
    (cond (empty? l) (reverse result)
          (= (first l) a) (concat (reverse result) (rest l))
          :else (recur (rest l) (cons (first l) result)))) )

;;;
;;;    (= '(1 2 3) [1 2 3]) => true
;;;    
(defn rember [a lat]
  (loop [l lat
         result []]
    (cond (empty? l) result
          (= (first l) a) (concat result (rest l))
          :else (recur (rest l) (conj result (first l)))) ))

(deftest test-rember
  (is (= (rember 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly)))
  (is (= (rember 'mint '(lamb chops and mint flavored mint jelly)) '(lamb chops and flavored mint jelly)))
  (is (= (rember 'toast '(bacon lettuce and tomato)) '(bacon lettuce and tomato)))
  (is (= (rember 'bacon '(bacon lettuce and tomato)) '(lettuce and tomato)))
  (is (= (rember 'and '(bacon lettuce and tomato)) '(bacon lettuce tomato)))
  (is (= (rember 'cup '(coffee cup tea cup and hick cup)) '(coffee tea cup and hick cup)))
  (is (= (rember 'sauce '(soy sauce and tomato sauce)) '(soy and tomato sauce))))

(defn firsts [lat]
  (if (empty? lat)
    '()
    (cons (first (first lat)) (firsts (rest lat)))) )

(deftest test-firsts
  (is (= (firsts '((apple peach pumpkin) (plum pear cherry) (grape raisin pea) (bean carrot eggplant))) '(apple plum grape bean)))
  (is (= (firsts '((a b) (c d) (e f))) '(a c e)))
  (is (= (firsts '((five plums) (four) (eleven green oranges))) '(five four eleven)))
  (is (= (firsts '()) '())))

(defn firsts [lat]
  (map first lat))

(defn insert-r [old new lat]
  (cond (empty? lat) '()
        (= (first lat) old) (cons old (cons new (rest lat)))
        :else (cons (first lat) (insert-r old new (rest lat)))) )

(defn insert-r [old new lat]
  (letfn [(insert [lat result]
            (if (empty? lat)
              (reverse result)
              (let [[head & tail] lat]
                (if (= head old)
                  (concat (reverse result) (list* old new tail))
                  (insert tail (cons head result)))) ))]
    (insert lat '())))

(defn insert-r [old new lat]
  (letfn [(insert [[head & tail :as lat] result]
            (cond (empty? lat) (reverse result)
                  (= head old) (concat (reverse result) (list* old new tail))
                  :else (insert tail (cons head result))))]
    (insert lat '())))

(defn insert-r [old new lat]
  (loop [lat lat
         result '()]
    (cond (empty? lat) (reverse result)
          (= (first lat) old) (concat (reverse result) (list* old new (rest lat)))
          :else (recur (rest lat) (cons (first lat) result)))) )
  
(defn insert-r [old new lat]
  (loop [[head & tail :as lat] lat
         result '()]
    (cond (empty? lat) (reverse result)
          (= head old) (concat (reverse result) (list* old new tail))
          :else (recur tail (cons head result)))) )
  
(deftest test-insert-r
  (is (= (insert-r 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert)))
  (is (= (insert-r 'and 'jalapeno '(tacos tamales and salsa)) '(tacos tamales and jalapeno salsa)))
  (is (= (insert-r 'd 'e '(a b c d f g d h)) '(a b c d e f g d h))))

(defn insert-l [old new lat]
  (cond (empty? lat) '()
        (= (first lat) old) (cons new lat)
        :else (cons (first lat) (insert-l old new (rest lat)))) ) 

(defn insert-l [old new lat]
  (letfn [(insert [lat result]
            (if (empty? lat)
              (reverse result)
              (let [[head & tail] lat]
                (if (= head old)
                  (concat (reverse result) (list* new old tail))
                  (insert tail (cons head result)))) ))]
    (insert lat '())))
  
(defn insert-l [old new lat]
  (letfn [(insert [[head & tail :as lat] result]
            (cond (empty? lat) (reverse result)
                  (= head old) (concat (reverse result) (list* new old tail))
                  :else (insert tail (cons head result))))]
    (insert lat '())))
  
(defn insert-l [old new lat]
  (loop [lat lat
         result '()]
    (cond (empty? lat) (reverse result)
          (= (first lat) old) (concat (reverse result) (cons new lat))
          :else (recur (rest lat) (cons (first lat) result)))) )
  
(defn insert-l [old new lat]
  (loop [[head & tail :as lat] lat
         result '()]
    (cond (empty? lat) (reverse result)
          (= head old) (concat (reverse result) (cons new lat)) ; Shortcut for (list* new old tail)
          :else (recur tail (cons head result)))) )
  
(deftest test-insert-l
  (is (= (insert-l 'topping 'fudge '(ice cream with topping for dessert)) '(ice cream with fudge topping for dessert)))
  (is (= (insert-l 'jalapeno 'and '(tacos tamales jalapeno salsa)) '(tacos tamales and jalapeno salsa)))
  (is (= (insert-l 'e 'd '(a b c e f g e h)) '(a b c d e f g e h))))

(defn subst [old new lat]
  (cond (empty? lat) '()
        (= (first lat) old) (cons new (rest lat))
        :else (cons (first lat) (subst old new (rest lat)))) )

(defn subst [old new lat]
  (letfn [(insert [lat result]
            (if (empty? lat)
              (reverse result)
              (let [[head & tail] lat]
                (if (= head old)
                  (concat (reverse result) (cons new tail))
                  (insert tail (cons head result)))) ))]
    (insert lat '())))

(defn subst [old new lat]
  (letfn [(insert [[head & tail :as lat] result]
            (cond (empty? lat) (reverse result)
                  (= head old) (concat (reverse result) (cons new tail))
                  :else (insert tail (cons head result))))]
    (insert lat '())))

(defn subst [old new lat]
  (loop [lat lat
         result '()]
    (cond (empty? lat) (reverse result)
          (= (first lat) old) (concat (reverse result) (cons new (rest lat)))
          :else (recur (rest lat) (cons (first lat) result)))) )

(defn subst [old new lat]
  (loop [[head & tail :as lat] lat
         result '()]
    (cond (empty? lat) (reverse result)
          (= head old) (concat (reverse result) (cons new tail))
          :else (recur tail (cons head result)))) )

(deftest test-subst
  (is (= (subst 'pickle 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge for dessert)))
  (is (= (subst 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with topping for dessert)))
  (is (= (subst 'and 'jalapeno '(tacos tamales and salsa)) '(tacos tamales jalapeno salsa)))
  (is (= (subst 'd 'e '(a b c d f g d h)) '(a b c e f g d h))))

(defn subst2 [old1 old2 new lat]
  (cond (empty? lat) '()
        (or (= (first lat) old1)
            (= (first lat) old2))
        (cons new (rest lat))
        :else (cons (first lat) (subst2 old1 old2 new (rest lat)))) )

(defn subst2 [old1 old2 new lat]
  (letfn [(insert [lat result]
            (if (empty? lat)
              (reverse result)
              (let [[head & tail] lat]
                (if (or (= head old1) (= head old2))
                  (concat (reverse result) (cons new tail))
                  (insert tail (cons head result)))) ))]
    (insert lat '())))

(defn subst2 [old1 old2 new lat]
  (letfn [(insert [[head & tail :as lat] result]
            (cond (empty? lat) (reverse result)
                  (or (= head old1) (= head old2)) (concat (reverse result) (cons new tail))
                  :else (insert tail (cons head result))))]
    (insert lat '())))

(defn subst2 [old1 old2 new lat]
  (loop [lat lat
         result '()]
    (cond (empty? lat) (reverse result)
          (or (= (first lat) old1) (= (first lat) old2)) (concat (reverse result) (cons new (rest lat)))
          :else (recur (rest lat) (cons (first lat) result)))) )

(defn subst2 [old1 old2 new lat]
  (loop [[head & tail :as lat] lat
         result '()]
    (cond (empty? lat) (reverse result)
          (or (= head old1) (= head old2)) (concat (reverse result) (cons new tail))
          :else (recur tail (cons head result)))) )

(deftest test-subst2
  (is (= (subst2 'pickle 'vinegar 'vanilla '(banana ice cream with chocolate topping))
         '(banana ice cream with chocolate topping)))
  (is (= (subst2 'chocolate 'banana 'vanilla '(banana ice cream with chocolate topping))
         '(vanilla ice cream with chocolate topping)))
  (is (= (subst2 'banana 'peach 'vanilla '(chocolate topping on banana ice cream))
         '(chocolate topping on vanilla ice cream))))

(defn insert [match? next-result lat result]
  (if (empty? lat)
    (reverse result)
    (let [[head & tail] lat]
      (if (match? head)
        (reverse (concat (reverse tail) (next-result result)))
        (insert match? next-result tail (cons head result)))) ))

(defn insert [match? next-result [head & tail :as lat] result]
  (if (empty? lat)
    (reverse result)
    (if (match? head)
      (reverse (concat (reverse tail) (next-result result)))
      (insert match? next-result tail (cons head result)))) )

(defn insert-r [old new lat]
  (insert (fn [elt] (= elt old))
          (fn [result] (list* new old result)) ; This appears to be backwards since result gets reversed.
          lat
          '()))

(defn insert-l [old new lat]
  (insert (fn [elt] (= elt old))
          (fn [result] (list* old new result))
          lat
          '()))

(defn subst [old new lat]
  (insert (fn [elt] (= elt old))
          (fn [result] (cons new result))
          lat
          '()))

(defn subst2 [old1 old2 new lat]
  (insert (fn [elt] (or (= elt old1) (= elt old2)))
          (fn [result] (cons new result))
          lat
          '()))

(defn insert [match? next-result lat]
  (loop [[head & tail :as lat] lat
         result '()]
    (if (empty? lat)
      (reverse result)
      (if (match? head)
        (reverse (concat (reverse tail) (next-result result)))
        (recur tail (cons head result)))) ))

(defn insert-r [old new lat]
  (insert (fn [elt] (= elt old))
          (fn [result] (list* new old result))
          lat))

(defn insert-l [old new lat]
  (insert (fn [elt] (= elt old))
          (fn [result] (list* old new result))
          lat))


(defn subst [old new lat]
  (insert (fn [elt] (= elt old))
          (fn [result] (cons new result))
          lat))

(defn subst2 [old1 old2 new lat]
  (insert (fn [elt] (or (= elt old1) (= elt old2)))
          (fn [result] (cons new result))
          lat))

;;;
;;;    Ch. 4
;;;
(defn plus [m n]
  (if (zero? n)
    m
    (plus (inc m) (dec n))))

(deftest test+
  (is (== (plus 46 12) (+ 46 12))))

(defn minus [m n]
  (if (zero? n)
    m
    (minus (dec m) (dec n))))

(deftest test-
  (is (== (minus 8 3) (- 8 3)))
  (is (== (minus 17 9) (- 17 9))))

(defn addvec [vec]
  (if (empty? vec)
    0
    (+ (first vec) (addvec (rest vec)))) )

(defn addvec [vec]
  (loop [v vec
         sum 0]
    (if (empty? v)
      sum
      (recur (rest v) (+ sum (first v)))) ))

(deftest test-addvec
  (let [v '(3 5 2 8)]
    (is (== (addvec v) (reduce + v))))
  (let [v '(15 6 7 12 3)]
    (is (== (addvec v) (reduce + v)))) )

(defn product [m n]
  (if (zero? n)
    0
    (plus (product m (dec n)) m)))

(deftest test-product
  (is (== (product 5 3) (* 5 3)))
  (is (== (product 13 4) (* 13 4)))
  (is (== (product 12 3) (* 12 3))))

(defn vec+ [v1 v2]
  (cond (and (empty? v1) (empty? v2)) '()
        (or (empty? v1) (empty? v2)) (throw (Exception. "Length mismatch"))
        :else (cons (plus (first v1) (first v2))
                    (vec+ (rest v1) (rest v2)))) )

(deftest test-vec+
  (let [v1 '(3 6 9 11 4)
        v2 '(8 5 2 0 7)]
    (is (= (vec+ v1 v2) (map + v1 v2))))
  (let [v1 '(3 7)
        v2 '(4 6)]
    (is (= (vec+ v1 v2) (map + v1 v2))))
  (let [v1 '(2 3)
        v2 '(4 6)]
    (is (= (vec+ v1 v2) (map + v1 v2)))) )


  
