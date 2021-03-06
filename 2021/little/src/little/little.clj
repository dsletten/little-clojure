;;;;
;;;;
;;;;   I think of Clojure as kind of the greatest hits of the last 20 or 30 years of computer science. It's like that mix tape from the Guardians of the Galaxy, only in software.
;;;;   -- Russ Olsen
;;;;
;;;;   Name:               little.clj
;;;;
;;;;   Started:            Fri Apr 30 11:20:42 2021
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

;(run-tests 'little 'unary)

(ns little
  (:use clojure.test
        [clojure.pprint :only (cl-format)]))

;;;
;;;    Chapter 2
;;;    

;;;
;;;    Different semantics than The Little Schemer!
;;;    Little Lisper pg. 5: () is both a list and an atom.
;;;    
(defn atom? [o]
  (or (symbol? o) (number? o) (keyword? o) (boolean? o) (and (list? o) (empty? o))))

(defn lat? [l]
  (cond (empty? l) true
        (atom? (first l)) (recur (rest l))
        :else false))

;;;
;;;    Defeats the point of the book...
;;;    
;; (defn lat? [l]
;;   (every? atom? l))

(deftest test-lat?
  (is (lat? '(jack sprat could eat no chicken fat)))
  (is (not (lat? '((jack) sprat could eat no chicken fat))))
  (is (not (lat? '(jack (sprat could) eat no chicken fat))))
  (is (lat? '()))
  (is (lat? '(bacon and eggs)))
  (is (not (lat? '(bacon (and eggs)))) ))

;;;
;;;    Book's version (essentially)
;;;    
;; (defn member [a lat & {:keys [test] :or {test =}}]
;;   (cond (empty? lat) false
;;         :else (or (test (first lat) a)
;;                   (recur a (rest lat) :test test))) )

(defn member [a lat & {:keys [test] :or {test =}}]
  (cond (empty? lat) false
        :else (or (test (first lat) a)
                  (recur a (rest lat) [:test test]))) )

;; (defn member [a lat & {:keys [test] :or {test =}}]
;;   (loop [lat lat]
;;     (cond (empty? lat) false
;;           :else (or (test (first lat) a)
;;                     (recur (rest lat)))) ))

(defn member [a lat & {:keys [test] :or {test =}}] ; Keyword arg perhaps not necessary in Clojure: (= '((a b) (c d)) '((a b) (c d))) => true
  (cond (empty? lat) false
        (test (first lat) a) true
        :else (recur a (rest lat) [:test test])))

;; (defn member [a lat & {:keys [test] :or {test =}}]
;;   (loop [lat lat]
;;     (cond (empty? lat) false
;;           (test (first lat) a) true
;;           :else (recur (rest lat)))) )

(deftest test-member
  (is (member 'tea '(coffee tea or milk)))
  (is (not (member 'poached '(fried eggs and scrambled eggs))))
  (is (member 'meat '(mashed potatoes and meat gravy)))
  (is (not (member 'liver '(bagels and lox)))) )

;;;
;;;    Chapter 3
;;;    

;; (defn rember [a lat]
;;   (cond (empty? lat) '()
;;         :else (cond (= (first lat) a) (rest lat)
;;                     :else (cons (first lat) (rember a (rest lat)))) ))

(defn rember [a lat]
  (cond (empty? lat) '()
        (= (first lat) a) (rest lat)
        :else (cons (first lat) (rember a (rest lat)))) )

(deftest test-rember
  (is (= (rember 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly)))
  (is (= (rember 'mint '(lamb chops and mint flavored mint jelly)) '(lamb chops and flavored mint jelly)))
  (is (= (rember 'toast '(bacon lettuce and tomato)) '(bacon lettuce and tomato)))
  (is (= (rember 'cup '(coffee cup tea cup and hick cup)) '(coffee tea cup and hick cup)))
  (is (= (rember 'bacon '(bacon lettuce and tomato)) '(lettuce and tomato)))
  (is (= (rember 'and '(bacon lettuce and tomato)) '(bacon lettuce tomato)))
  (is (= (rember 'sauce '(soy sauce and tomato sauce)) '(soy and tomato sauce))))

(defn firsts [lol]
  (cond (empty? lol) '()
        :else (cons (ffirst lol) (firsts (rest lol)))) )

(defn firsts [lol]
  (if (empty? lol)
      '()
      (let [[[first & _] & lists] lol]
        (cons first (firsts lists)))) )

(deftest test-firsts
  (is (= (firsts '((apple peach pumpkin) (plum pear cherry) (grape raisin pea) (bean carrot eggplant))) '(apple plum grape bean)))
  (is (= (firsts '((a b) (c d) (e f))) '(a c e)))
  (is (= (firsts '()) '()))
  (is (= (firsts '((five plums) (four) (eleven green oranges))) '(five four eleven))))

(defn insertr [old new lat]
  (cond (empty? lat) '()
        (= (first lat) old) (cons old (cons new (rest lat)))) ; <-------------  Bad )))!!!!
        :else (cons (first lat) (insertr old new (rest lat))))

(defn insertr [old new lat]
  (cond (empty? lat) '()
        (= (first lat) old) (cons old (cons new (rest lat)))
        :else (cons (first lat) (insertr old new (rest lat)))) )

(defn insertr [old new lat]
  (cond (empty? lat) '()
        (= (first lat) old) (list* old new (rest lat))
        :else (cons (first lat) (insertr old new (rest lat)))) )

(deftest test-insertr
  (is (= (insertr 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert)))
  (is (= (insertr 'and 'jalape??o '(tacos tamales and salsa)) '(tacos tamales and jalape??o salsa)))
  (is (= (insertr 'd 'e '(a b c d f g d h)) '(a b c d e f g d h))))

(defn insertl [old new lat]
  (cond (empty? lat) '()
        (= (first lat) old) (cons new lat)
        :else (cons (first lat) (insertl old new (rest lat)))) )

(deftest test-insertl
  (is (= (insertl 'topping 'fudge '(ice cream with topping for dessert)) '(ice cream with fudge topping for dessert)))
  (is (= (insertl 'salsa 'jalape??o '(tacos tamales and salsa)) '(tacos tamales and jalape??o salsa)))
  (is (= (insertl 'f 'e '(a b c d f g d h)) '(a b c d e f g d h))))

(defn subst [old new lat]
  (cond (empty? lat) '()
        (= (first lat) old) (cons new (rest lat))
        :else (cons (first lat) (subst old new (rest lat)))) )

(deftest test-subst
  (is (= (subst 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with topping for dessert)))
  (is (= (subst 'jalape??o 'and '(tacos tamales jalape??o salsa)) '(tacos tamales and salsa)))
  (is (= (subst 'd 'e '(a b c d f g d h)) '(a b c e f g d h))))

;;;
;;;    Refactor! (Compare insert-g below)
;;;    
(defn skeleton [f target lat]
  (cond (empty? lat) '()
        (= (first lat) target) (f lat)
        :else (cons (first lat) (skeleton f target (rest lat)))) )

(defn rember [a lat]
  (skeleton rest a lat))

(defn insertr [old new lat]
  (skeleton #(list* old new (rest %)) old lat))

(defn insertl [old new lat]
  (skeleton #(cons new %) old lat))

(defn subst [old new lat]
  (skeleton #(cons new (rest %)) old lat))

(defn subst2
  "Substitute for the first instance of either `o1` or `o2` with `new`."
  [o1 o2 new lat]
  (cond (empty? lat) '()
        (or (= (first lat) o1)
            (= (first lat) o2))
        (cons new (rest lat))
        :else (cons (first lat) (subst2 o1 o2 new (rest lat)))) )

(deftest test-subst2
  (is (= (subst2 'chocolate 'banana 'vanilla '(banana ice cream with chocolate topping)) '(vanilla ice cream with chocolate topping))))

;;;
;;;    Refactor again! (Previous change cannot accomodate subst2)
;;;    
(defn skeleton2 [match action lat]
  (cond (empty? lat) '()
        (match (first lat)) (action lat)
        :else (cons (first lat) (skeleton2 match action (rest lat)))) )

(defn rember [a lat]
  (skeleton2 #(= % a) rest lat))

(defn insertr [old new lat]
  (skeleton2 #(= % old) #(list* old new (rest %)) lat))

(defn insertl [old new lat]
  (skeleton2 #(= % old) #(cons new %) lat))

(defn subst [old new lat]
  (skeleton2 #(= % old) #(cons new (rest %)) lat))

(defn subst2 [o1 o2 new lat]
;  (skeleton2 #(or (= % o1) (= % o2)) #(cons new (rest %)) lat))
  (skeleton2 #(contains? #{o1 o2} %) #(cons new (rest %)) lat))

;;;
;;;    Chapter 4
;;;

(defn + [n m]
  (cond (zero? n) m
;        :else (+ (dec n) (inc m))))
        :else (recur (dec n) (inc m))))

(deftest test-+
  (is (== (+ 0 0) (+ 0 0)))
  (is (== (+ 0 1) (+ 0 1)))
  (is (== (+ 1 0) (+ 1 0)))
  (is (== (+ 1 1) (+ 1 1)))
  (is (== (+ 2 3) (+ 2 3))))

(defn - [n m]
  (cond (zero? m) n
        (zero? n) (throw (IllegalArgumentException. "Negative numbers are not supported."))
;        :else (- (dec n) (dec m))))
        :else (recur (dec n) (dec m))))

(deftest test--
  (is (== (- 0 0) (- 0 0)))
  (is (== (- 1 0) (- 1 0)))
  (is (== (- 1 1) (- 1 1)))
  (is (== (- 8 3) (- 8 3)))
  (is (== (- 17 9) (- 17 9)))
  (try (- 3 5)
       (catch Exception _ :eof)))

(defn addvec [vec]
  (cond (empty? vec) 0
        :else (+ (first vec) (addvec (rest vec)))) )

(deftest test-addvec
  (is (== (addvec '()) (reduce clojure.core/+ '())))
  (is (== (addvec '(3 5 2 8)) (reduce + '(3 5 2 8))))
  (is (== (addvec '(15 6 7 12 3)) (reduce + '(15 6 7 12 3)))) )

(defn * [n m]
  (cond (zero? m) 0
        :else (+ n (* n (dec m)))) )

(deftest test-*
  (is (== (* 5 3) (* 5 3)))
  (is (== (* 13 4) (* 13 4)))
  (is (== (* 12 3) (* 12 3))))

(defn vec+ [vec1 vec2]
  (cond (empty? vec1) '()
        :else (cons (+ (first vec1) (first vec2))
                    (vec+ (rest vec1) (rest vec2)))) )

(defn vec+ [xs ys]
  (cond (empty? xs) '()
        :else (let [[x & morex] xs
                    [y & morey] ys]
                (cons (+ x y) (vec+ morex morey)))) )

(defn vec+ [xs ys]
  (map + xs ys))

(deftest test-vec+
  (is (= (vec+ '(3 6 9 11 4) '(8 5 2 0 7)) (map + '(3 6 9 11 4) '(8 5 2 0 7))))
  (is (= (vec+ '(2 3) '(4 6)) (map + '(2 3) '(4 6))))
  (is (= (vec+ '(3 7) '(4 6)) (map + '(3 7) '(4 6)))))

(defn vec+ [vec1 vec2]
  (cond (empty? vec1) vec2
        (empty? vec2) vec1
        :else (cons (+ (first vec1) (first vec2))
                    (vec+ (rest vec1) (rest vec2)))) )

(deftest test-vec+
  (is (= (vec+ '(3 6 9 11 4) '(8 5 2 0 7)) (map + '(3 6 9 11 4) '(8 5 2 0 7))))
  (is (= (vec+ '(2 3) '(4 6)) (map + '(2 3) '(4 6))))
  (is (= (vec+ '(3 7) '(4 6)) (map + '(3 7) '(4 6))))
  (is (= (vec+ '(3 7) '(4 6 8 1)) '(7 13 8 1)))
  (is (= (vec+ '(3 7 8 1) '(4 6)) '(7 13 8 1))))

(defn > [n m]
  (cond (zero? n) false
        (zero? m) true
        :else (recur (dec n) (dec m))))

(deftest test->
  (is (not (> 0 1)))
  (is (> 1 0))
  (is (not (> 12 13)))
  (is (not (> 12 12)))
  (is (> 12 11)))

(defn < [n m]
  (> m n))

(defn < [n m]
  (cond (zero? m) false
        (zero? n) true
        :else (recur (dec n) (dec m))))

(deftest test-<
  (is (< 0 1))
  (is (not (< 1 0)))
  (is (< 4 6))
  (is (not (< 8 3)))
  (is (not (< 6 6))))

(defn == [n m]
  (cond (zero? n) (zero? m)
        (zero? m) false
        :else (recur (dec n) (dec m))))

(defn == [n m]
  (cond (> n m) false
        (< n m) false
        :else true))

(deftest test-==
  (is (== 0 0))
  (is (not (== 0 1)))
  (is (not (== 1 0)))
  (is (== 2 2))
  (is (not (== 2 3)))
  (is (not (== 3 2))))

(defn expt [n m]
  (cond (zero? m) 1
        :else (* n (expt n (dec m)))) )

(deftest test-expt
  (is (== (expt 1 1) (Math/pow 1 1)))
  (is (== (expt 2 3) (Math/pow 2 3)))
  (is (== (expt 5 3) (Math/pow 5 3))))

(defn length [lat]
  (cond (empty? lat) 0
        :else (inc (length (rest lat)))) )

(deftest test-length
  (is (== (length '(hotdogs with mustard sauerkraut and pickles)) (count '(hotdogs with mustard sauerkraut and pickles))))
  (is (== (length '(ham and cheese on rye)) (count '(ham and cheese on rye)))) )

;;;
;;;    1-based index!
;;;    
(defn pick [n lat]
  (cond (empty? lat) nil
        (== n 1) (first lat)
        :else (recur (dec n) (rest lat))))

(deftest test-pick ()
  (is (= (pick 4 '(lasagna spaghetti ravioli macaroni meatball)) 'macaroni))
  (is (= (pick 0 '()) nil)))

;;;
;;;    1-based index!
;;;    
(defn rempick [n lat]
  (cond (empty? lat) '()
        (== n 1) (rest lat)
        :else (cons (first lat) (rempick (dec n) (rest lat)))) )

(deftest test-rempick
  (is (= (rempick 3 '(hotdogs with hot mustard)) '(hotdogs with mustard)))
  (is (= (rempick 0 '()) '())))

(defn no-nums [lat]
  (cond (empty? lat) '()
        (number? (first lat)) (no-nums (rest lat))
        :else (cons (first lat) (no-nums (rest lat)))) )

(deftest test-no-nums
  (is (= (no-nums '(5 pears 6 prunes 9 dates)) (remove number? '(5 pears 6 prunes 9 dates)))) )

(defn all-nums [lat]
  (cond (empty? lat) '()
        (number? (first lat)) (cons (first lat) (all-nums (rest lat)))
        :else (all-nums (rest lat))))

(deftest test-all-nums
  (is (= (all-nums '(5 pears 6 prunes 9 dates)) (filter number? '(5 pears 6 prunes 9 dates)))) )

;;;
;;;    Assumes that A1 and A2 are both atoms.
;;;    
(defn eqatom [a1 a2]
  (cond (number? a1) (and (number? a2) (== a1 a2))
        (number? a2) false
        :else (= a1 a2)))

(deftest test-eqatom
  (is (eqatom 3 3))
  (is (eqatom 'pung 'pung))
  (is (not (eqatom 3 4)))
  (is (not (eqatom 'pung 'foo)))
  (is (not (eqatom 3 'pung)))
  (is (not (eqatom 'pung 3))))

;;;
;;;    Chapter 5
;;;

(defn multirember
  "Remove all instances of `a` from `lat`."
  [a lat]
  (cond (empty? lat) '()
        (= (first lat) a) (multirember a (rest lat))
        :else (cons (first lat) (multirember a (rest lat)))) )

(deftest test-multirember
  (is (= (multirember 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly)))
  (is (= (multirember 'mint '(lamb chops and mint flavored mint jelly)) '(lamb chops and flavored jelly)))
  (is (= (multirember 'toast '(bacon lettuce and tomato)) '(bacon lettuce and tomato)))
  (is (= (multirember 'cup '(coffee cup tea cup and hick cup)) '(coffee tea and hick)))
  (is (= (multirember 'bacon '(bacon lettuce and tomato)) '(lettuce and tomato)))
  (is (= (multirember 'and '(bacon lettuce and tomato)) '(bacon lettuce tomato)))
  (is (= (multirember 'sauce '(soy sauce and tomato sauce)) '(soy and tomato))))

(defn multiinsertr [old new lat]
  (cond (empty? lat) '()
        (= (first lat) old) (cons old (cons new (multiinsertr old new (rest lat))))
        :else (cons (first lat) (multiinsertr old new (rest lat)))) )

(defn multiinsertr [old new lat]
  (cond (empty? lat) '()
        (= (first lat) old) (list* old new (multiinsertr old new (rest lat)))
        :else (cons (first lat) (multiinsertr old new (rest lat)))) )

(deftest test-multiinsertr
  (is (= (multiinsertr 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert)))
  (is (= (multiinsertr 'and 'jalape??o '(tacos and tamales and salsa)) '(tacos and jalape??o tamales and jalape??o salsa)))
  (is (= (multiinsertr 'd 'e '(a b c d f g d h)) '(a b c d e f g d e h))))

(defn multiinsertl [old new lat]
  (cond (empty? lat) '()
        (= (first lat) old) (cons new (cons old (multiinsertl old new (rest lat))))
        :else (cons (first lat) (multiinsertl old new (rest lat)))) )

(defn multiinsertl [old new lat]
  (cond (empty? lat) '()
        (= (first lat) old) (list* new old (multiinsertl old new (rest lat)))
        :else (cons (first lat) (multiinsertl old new (rest lat)))) )

(deftest test-multiinsertl
  (is (= (multiinsertl 'topping 'fudge '(ice cream with topping for dessert)) '(ice cream with fudge topping for dessert)))
  (is (= (multiinsertl 'salsa 'jalape??o '(tacos tamales and salsa)) '(tacos tamales and jalape??o salsa)))
  (is (= (multiinsertl 'f 'e '(a b c d f g d f h)) '(a b c d e f g d e f h)))
  (is (= (multiinsertl 'fish 'fried '(chips and fish or fish and fried)) '(chips and fried fish or fried fish and fried))))

(defn multisubst [old new lat]
  (cond (empty? lat) '()
        (= (first lat) old) (cons new (multisubst old new (rest lat)))
        :else (cons (first lat) (multisubst old new (rest lat)))) )

(deftest test-multisubst
  (is (= (multisubst 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with topping for dessert)))
  (is (= (multisubst 'jalape??o 'and '(tacos tamales jalape??o salsa)) '(tacos tamales and salsa)))
  (is (= (multisubst 'd 'e '(a b c d f g d h)) '(a b c e f g e h))))

(defn occur [a lat]
  (cond (empty? lat) 0
        (= (first lat) a) (inc (occur a (rest lat)))
        :else (occur a (rest lat))))

(deftest test-occur
  (is (zero? (occur 'a '())))
  (is (== (occur 'a '(a b c)) 1))
  (is (== (occur 'a '(a a a)) 3))
  (is (== (occur 'a '(a b a c a)) 3)))

(defn one? [n]
  (cond (zero? n) false
        :else (zero? (dec n))))

(defn one? [n]
  (== n 1))

(deftest test-one?
  (is (one? 1))
  (is (not (one? 0)))
  (is (not (one? 2))))
   
;;;
;;;    1-based index!
;;;    
(defn rempick [n lat]
  (cond (empty? lat) '()
        (one? n) (rest lat)
        :else (cons (first lat) (rempick (dec n) (rest lat)))) )

(defn multisubst2 [o1 o2 new lat]
  "Substitute for all instances of either O1 or O2 with NEW."
  (cond (empty? lat) '()
        (or (= (first lat) o1)
            (= (first lat) o2))
         (cons new (multisubst2 o1 o2 new (rest lat)))
        :else (cons (first lat) (multisubst2 o1 o2 new (rest lat)))) )

(defn multisubst2
  ([olds new lat]
   (cond (empty? lat) '()
         (contains? olds (first lat)) (cons new (multisubst2 olds new (rest lat)))
         :else (cons (first lat) (multisubst2 olds new (rest lat)))) )
  ([o1 o2 new lat] (multisubst2 #{o1 o2} new lat)))

(deftest test-multisubst2
  (is (= (multisubst2 'chocolate 'banana 'vanilla '(banana ice cream with chocolate topping)) '(vanilla ice cream with vanilla topping))))

(defn multiskeleton [match action lat]
  (cond (empty? lat) '()
        (match (first lat)) (action (multiskeleton match action (rest lat)))
        :else (cons (first lat) (multiskeleton match action (rest lat)))) )

(defn multirember [a lat]
  (multiskeleton #(= % a) identity lat))

(defn multiinsertr [old new lat]
  (multiskeleton #(= % old) #(list* old new %) lat))

(defn multiinsertl [old new lat]
  (multiskeleton #(= % old) #(list* new old %) lat))

(defn multisubst [old new lat]
  (multiskeleton #(= % old) #(cons new %) lat))

(defn multisubst2 [o1 o2 new lat]
  (multiskeleton #(contains? #{o1 o2} %) #(cons new %) lat))

;;;
;;;    Chapter 6
;;;
(defn leftmost
  "Return the leftmost atom in `l`."
  [l]
  (cond (empty? l) '()
        (atom? (first l)) (first l)
        :else (leftmost (first l))))

(deftest test-leftmost
  (is (= (leftmost '((hot) (tuna (and)) cheese)) 'hot))
  (is (= (leftmost '(((hamburger) french) (fries (and a) coke))) 'hamburger))
  (is (= (leftmost '((((4) four)) 17 (seventeen))) 4))
  (is (= (leftmost '(((() four)) 17 (seventeen))) '())))

(defn non-atom? [obj]
  (not (atom? obj)))

(defn rember* [a l]
  (cond (empty? l) '()
        (atom? (first l)) (cond (= (first l) a) (rember* a (rest l))
                                :else (cons (first l) (rember* a (rest l))))
        :else (cons (rember* a (first l))
                    (rember* a (rest l)))) )

(defn rember* [a l]
  (cond (empty? l) '()
        :else (let [[head & tail] l]
                (cond (atom? head) (if (= head a)
                                     (rember* a tail)
                                     (cons head (rember* a tail)))
                      :else (cons (rember* a head)
                                  (rember* a tail)))) ))

(deftest test-rember*
  (is (= (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup)) '((coffee) ((tea)) (and (hick)))) )
  (is (= (rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce))) '(((tomato)) ((bean)) (and ((flying)))) )))

(defn occur* [a l]
  (cond (empty? l) 0
        (atom? (first l)) (cond (= (first l) a) (inc (occur* a (rest l)))
                                :else (occur* a (rest l)))
        :else (+ (occur* a (first l))
                 (occur* a (rest l)))) )

(deftest test-occur*
  (is (== (occur* 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy))) 5)))

(defn subst* [old new l]
  (cond (empty? l) '()
        (atom? (first l)) (cond (= (first l) old) (cons new (subst* old new (rest l)))
                                :else (cons (first l) (subst* old new (rest l))))
        :else (cons (subst* old new (first l))
                    (subst* old new (rest l)))) )

(deftest test-subst*
  (is (= (subst* 'banana 'orange '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))
         '((orange) (split ((((orange ice))) (cream (orange)) sherbet)) (orange) (bread) (orange brandy)))) )

(defn insertl* [old new l]
  (cond (empty? l) '()
        (atom? (first l)) (cond (= (first l) old) (cons new (cons old (insertl* old new (rest l))))
                                :else (cons (first l) (insertl* old new (rest l))))
        :else (cons (insertl* old new (first l))
                    (insertl* old new (rest l)))) )

(deftest test-insertl*
  (is (= (insertl* 'chuck 'pecker '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
         '((how much (wood)) could ((a (wood) pecker chuck)) (((pecker chuck))) (if (a) ((wood pecker chuck))) could pecker chuck wood))))

(defn member* [a l]
  (cond (empty? l) false
        (atom? (first l)) (cond (= (first l) a) true
                                :else (member* a (rest l)))
        :else (or (member* a (first l))
                  (member* a (rest l)))) )

(defn member* [a l]
  (cond (empty? l) false
        (atom? (first l)) (or (= (first l) a) (member* a (rest l)))
        :else (or (member* a (first l))
                  (member* a (rest l)))) )

(deftest test-member*
  (is (member* 'chips '((potato) (chips ((with) fish) (chips)))) ))

(defn skeleton* [base match fail action branch tree]
  (cond (empty? tree) base
        (atom? (first tree)) (if (match (first tree))
                               (action (skeleton* base match fail action branch (rest tree)))
                               (fail (first tree) (skeleton* base match fail action branch (rest tree))))
        :else (branch (skeleton* base match fail action branch (first tree))
                      (skeleton* base match fail action branch (rest tree)))) )

(defn rember* [a l]
  (skeleton* '() #(= % a) cons identity cons l))

(defn insertr* [old new l]
  (skeleton* '() #(= % old) cons #(list* old new %) cons l))

(defn occur* [a l]
  (skeleton* 0 #(= % a) (fn [elt rest] rest) inc + l))

(defn subst* [old new l]
  (skeleton* '() #(= % old) cons #(cons new %) cons l))

(defn insertl* [old new l]
  (skeleton* '() #(= % old) cons #(list* new old %) cons l))

(defn member* [a l]
  (skeleton* nil #(= % a) (fn [elt rest] rest) (fn [rest] true) #(or %1 %2) l))

(defn eqlist [l1 l2]
  (cond (empty? l1) (empty? l2)
        (empty? l2) false
        (atom? (first l1)) (and (atom? (first l2))
                                (eqatom (first l1) (first l2))
                                (eqlist (rest l1) (rest l2)))
        (atom? (first l2)) false
        :else (and (eqlist (first l1) (first l2))
                   (eqlist (rest l1) (rest l2)))) )

(defn eqlist [l1 l2]
  (cond (empty? l1) (empty? l2)
        (empty? l2) false
        (and (atom? (first l1)) (atom? (first l2))) (and (eqatom (first l1) (first l2)) (eqlist (rest l1) (rest l2)))
        (or (atom? (first l1)) (atom? (first l2))) false
        :else (and (eqlist (first l1) (first l2))
                   (eqlist (rest l1) (rest l2)))) )

(deftest test-eqlist
  (is (eqlist '(strawberry ice cream) '(strawberry ice cream)))
  (is (not (eqlist '(strawberry ice cream) '(strawberry cream ice))))
  (is (not (eqlist '(banana ((split))) '((banana) (split)))) )
  (is (not (eqlist '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda)))) ))
  (is (eqlist '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))) )
  (is (not (eqlist '((a) b) '(a b)))) )
   
(defn equal [o1 o2]
  (cond (and (atom? o1) (atom? o2)) (eqatom o1 o2)
        (or (atom? o1) (atom? o2)) false
        :else (eqlist o1 o2)))

(deftest test-equal
  (is (equal 3 3))
  (is (equal 'a 'a))
  (is (not (equal 2 3)))
  (is (not (equal 'a 3)))
  (is (not (equal 3 '(3))))
  (is (equal '(a b c) '(a b c)))
  (is (equal '((a) b ((c))) '((a) b ((c)))) )
  (is (not (equal '(a b ((c))) '((a) b ((c)))) )))

;;;
;;;    Mutually recursive.
;;;    
(defn eqlist [l1 l2]
  (cond (empty? l1) (empty? l2)
        (empty? l2) false
        :else (and (equal (first l1) (first l2))
                   (equal (rest l1) (rest l2)))) )

;; (defn rember [s l]
;;   (cond (empty? l) '()
;;         (equal (first l) s) (rest l)
;;         :else (cons (first l) (rember s (rest l)))) )

;;;
;;;    Chapter 7
;;;

;;;
;;;    The book assumes that AEXP is a valid representation of an arithmetic expression.
;;;    
(defn numbered? [aexp]
  (cond (atom? aexp) (number? aexp)
        (or (= (second aexp) '+)
            (= (second aexp) '*)
            (= (second aexp) '**))
        (and (numbered? (first aexp)) (numbered? (second (rest aexp))))
        :else false))

(defn numberdp [aexp]
  (cond (atom? aexp) (number? aexp)
        :else (let [[op1 operator op2] aexp]
                (case operator
                  (+ * **) (and (numbered? op1) (numbered? op2))
                  false))))

(deftest test-numberedp
  (is (numbered? '(3 + (4 * 5))))
  (is (not (numbered? '(2 * sausage)))) )

(defn value [aexp]
  (cond (number? aexp) aexp
        (= (second aexp) '+) (+ (value (first aexp)) (value (second (rest aexp))))
        (= (second aexp) '*) (* (value (first aexp)) (value (second (rest aexp))))
        (= (second aexp) '**) (expt (value (first aexp)) (value (second (rest aexp)))) ))

(defn value [aexp]
  (cond (number? aexp) aexp
        :else (let [[op1 operator op2] aexp]
                (case operator
                  + (+ (value op1) (value op2))
                  * (* (value op1) (value op2))
                  ** (expt (value op1) (value op2)))) ))

;;;
;;;    See atom-to-function below.
;;;    
;; (defn operation [operator]
;;   (case operator
;;     + +
;;     * *
;;     ** expt))

(def operation {'+ +
                '* *
                '** expt})

(defn value [aexp]
  (cond (number? aexp) aexp
        :else (let [[op1 operator op2] aexp]
                ((operation operator) (value op1) (value op2)))) )

(deftest test-value
  (is (== (value '(1 + 3)) 4))
  (is (== (value '(1 + (3 * 4))) 13))
  (is (== (value '(1 + (3 ** 4))) 82)))

;;;
;;;    New representation for arithmetic expressions.
;;;
(defn value [aexp]
  (cond (number? aexp) aexp
        (= (first aexp) 'plus) (+ (value (second aexp)) (value (second (rest aexp))))
        (= (first aexp) 'times) (* (value (second aexp)) (value (second (rest aexp))))
        (= (first aexp) 'expt) (expt (value (second aexp)) (value (second (rest aexp)))) ))
        
(deftest test-value
  (is (== (value '(plus 1 3)) 4))
  (is (== (value '(plus 1 (times 3 4))) 13))
  (is (== (value '(plus 1 (expt 3 4))) 82)))

;;;
;;;    Refactoring
;;;
(defn first-sub-exp [[operator op1 op2]]
  op1)

(defn second-sub-exp [[operator op1 op2]]
  op2)

(defn operator [[operator op1 op2]]
  operator)

(defn value [aexp]
  (cond (number? aexp) aexp
        (= (operator aexp) 'plus) (+ (value (first-sub-exp aexp)) (value (second-sub-exp aexp)))
        (= (operator aexp) 'times) (* (value (first-sub-exp aexp)) (value (second-sub-exp aexp)))
        (= (operator aexp) 'expt) (expt (value (first-sub-exp aexp)) (value (second-sub-exp aexp)))) )

(def operation {'plus +
                'times *
                'expt expt})

(defn value [aexp]
  (cond (number? aexp) aexp
        :else (let [[operator op1 op2] aexp]
                ((operation operator) (value op1) (value op2)))) )

;;;
;;;    Returning to first representation!
;;;
(defn first-sub-exp [[op1 operator op2]]
  op1)

(defn second-sub-exp [[op1 operator op2]]
  op2)

(defn operator [[op1 operator op2]]
  operator)

;;;
;;;    Cosmetic changes... plus -> +, times -> *, expt -> **
;;;    
(defn value [aexp]
  (cond (number? aexp) aexp
        (= (operator aexp) '+) (+ (value (first-sub-exp aexp)) (value (second-sub-exp aexp)))
        (= (operator aexp) '*) (* (value (first-sub-exp aexp)) (value (second-sub-exp aexp)))
        (= (operator aexp) '**) (expt (value (first-sub-exp aexp)) (value (second-sub-exp aexp)))) )

(deftest test-value
  (is (== (value '(1 + 3)) 4))
  (is (== (value '(1 + (3 * 4))) 13))
  (is (== (value '(1 + (3 ** 4))) 82)))

;;;
;;;    New representation for numbers:
;;;    0 ()
;;;    1 (())
;;;    2 (() ())
;;;
(ns unary
  (:use clojure.test
        [clojure.pprint :only (cl-format)]))

(def ^:constant unit '())

(defn zero? [n]
  (empty? n))

(defn inc [n]
  (cons unit n))

(defn dec [n]
  (rest n))

(defn + [n m]
  (cond (zero? n) m
        :else (+ (dec n) (inc m))))

(deftest test-+
  (is (zero? (+ '() '())))
  (is (= (+ '() '(())) '(())))
  (is (= (+ '(()) '()) '(())))
  (is (= (+ '(()) '(())) '(()())))
  (is (= (+ '(()()) '(()()())) '(()()()()()))) )

(defn number? [n]
  (cond (zero? n) true
        (= (first n) unit) (number? (dec n))
        :else false))

(deftest test-number?
  (is (number? '()))
  (is (number? (+ '(()) '(()()))) )
  (is (not (number? '(a b c)))) )

;;;
;;;    Chapter 8
;;;
(in-ns 'little)

(defn set? [l]
  (cond (empty? l) true
        (member (first l) (rest l)) false
        :else (set? (rest l))))

(deftest test-set?
  (is (set? '(apples peaches pears plums)))
  (is (not (set? '(apple peaches apple plum))))
  (is (not (set? '(apple 3 pear 4 9 apple 3 4)))) )

(defn make-set [lat]
  (cond (empty? lat) '()
        (member (first lat) (rest lat)) (make-set (rest lat))
        :else (cons (first lat) (make-set (rest lat)))) )

;;;
;;;    Out of order to keep Clojure happy...
;;;    
(defn subset? [s1 s2]
  (cond (empty? s1) true
        (member (first s1) s2) (subset? (rest s1) s2)
        :else false))

(deftest test-subset?
  (is (subset? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings)))
  (is (not (subset? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish)))) )

(defn set-equal? [s1 s2]
  (and (subset? s1 s2) (subset? s2 s1)))

(deftest test-make-set
  (is (set-equal? (make-set '(apple peach pear peach plum apple lemon peach)) '(apple peach pear plum lemon)))
  (is (set-equal? (make-set '(apple 3 pear 4 9 apple 3 4)) '(apple 3 pear 4 9))))

(defn make-set [lat]
  (cond (empty? lat) '()
        :else (cons (first lat) (make-set (multirember (first lat) (rest lat)))) ))

(defn subset? [s1 s2]
  (or (empty? s1)
      (and (member (first s1) s2)
           (subset? (rest s1) s2))))

(defn intersect? [s1 s2]
  (cond (empty? s1) false
        (member (first s1) s2) true
        :else (intersect? (rest s1) s2)))

(deftest test-intersect?
  (is (intersect? '(tomatoes and macaroni) '(macaroni and cheese))))

(defn intersect? [s1 s2]
  (cond (empty? s1) false
        :else (or (member (first s1) s2)
                  (intersect? (rest s1) s2))))

(defn intersection [s1 s2]
  (cond (empty? s1) '()
        (member (first s1) s2) (cons (first s1) (intersection (rest s1) s2))
        :else (intersection (rest s1) s2)))

(deftest test-intersection
  (is (set-equal? (intersection '(tomatoes and macaroni) '(macaroni and cheese)) '(and macaroni))))

(defn intersection [s1 s2]
  (cond (empty? s1) '()
        (not (member (first s1) s2)) (intersection (rest s1) s2)
        :else (cons (first s1) (intersection (rest s1) s2))))

(defn union [s1 s2]
  (cond (empty? s1) s2
        (member (first s1) s2) (union (rest s1) s2)
        :else (cons (first s1) (union (rest s1) s2))))

(deftest test-union
  (is (set-equal? (union '(tomatoes and macaroni casserole) '(macaroni and cheese)) '(tomatoes and macaroni casserole and cheese))))

(defn complement [s1 s2]
  (cond (empty? s1) '()
        (member (first s1) s2) (complement (rest s1) s2)
        :else (cons (first s1) (complement (rest s1) s2))))

(deftest test-complement
  (is (set-equal? (complement '(a b) '(a b)) '()))
  (is (set-equal? (complement '(a b c) '(d e f)) '(a b c)))
  (is (set-equal? (complement '(b c a) '(a d e)) '(b c))))

(defn intersectall [l-set]
  (cond (empty? l-set) '()
        (empty? (rest l-set)) (first l-set)
        :else (intersection (first l-set) (intersectall (rest l-set)))) )

(deftest test-intersectall
  (is (set-equal? (intersectall '((a b c) (c a d e) (e f g h a b))) '(a)))
  (is (set-equal? (intersectall '((6 pears and) (3 peaches and 6 peppers) (8 pears and 6 plums) (and 6 prunes with lots of apples))) '(6 and))))

(defn build [a1 a2]
  (cons a1 (cons a2 '())))

(defn fun? [rel]
  (set? (firsts rel)))

(deftest test-fun?
  (is (not (fun? '((4 3) (4 2) (7 6) (6 2) (3 4)))) )
  (is (fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))) )

(defn revrel [rel]
  (cond (empty? rel) '()
        :else (cons (build (second (first rel)) (first (first rel))) (revrel (rest rel)))) ) ; Ugh!!!

(defn revrel [rel]
  (cond (empty? rel) '()
        :else (let [[[a b] & more] rel]
             (cons (build b a) (revrel more)))) )

(deftest test-revrel
   (is (set-equal? (revrel '((8 a) (pumpkin pie) (got sick))) (map reverse '((8 a) (pumpkin pie) (got sick)))) ))

(defn injective? [fun]
  (set? (firsts (revrel fun))))

;;;
;;;    Book's version
;;;    
(defn injective? [fun]
  (fun? (revrel fun)))

(deftest test-injective?
  (is (not (injective? '((8 3) (4 2) (7 6) (6 2) (3 4)))) )
  (is (injective? '((8 3) (4 8) (7 6) (6 2) (3 4))))
  (is (not (injective? '((grape raisin) (plum prune) (stewed prune)))) )
  (is (injective? '((grape raisin) (plum prune) (stewed grape)))) )

;;;
;;;    Chapter 9
;;;

(defn rember-f [test a l]
  (cond (empty? l) '()
        (test (first l) a) (rest l)
        :else (cons (first l) (rember-f test a (rest l)))) )

(deftest test-rember-f
  (is (equal (rember-f = 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly)))
  (is (equal (rember-f = 'mint '(lamb chops and mint flavored mint jelly)) '(lamb chops and flavored mint jelly)))
  (is (equal (rember-f = 'toast '(bacon lettuce and tomato)) '(bacon lettuce and tomato)))
  (is (equal (rember-f = 'cup '(coffee cup tea cup and hick cup)) '(coffee tea cup and hick cup)))
  (is (equal (rember-f = 'bacon '(bacon lettuce and tomato)) '(lettuce and tomato)))
  (is (equal (rember-f = 'and '(bacon lettuce and tomato)) '(bacon lettuce tomato)))
  (is (equal (rember-f = 'sauce '(soy sauce and tomato sauce)) '(soy and tomato sauce)))
  (is (equal (rember-f == 5 '(6 2 5 3)) '(6 2 3)))
  (is (equal (rember-f = 'jelly '(jelly beans are good)) '(beans are good)))
  (is (equal (rember-f equal '(pop corn) '(lemonade (pop corn) and (cake))) '(lemonade and (cake)))) )

(defn rember-fn [test]
  (fn [a l]
    (cond (empty? l) '()
          (test (first l) a) (rest l)
          :else (cons (first l) ((rember-fn test) a (rest l)))) ))

(defn rember-fn [test]
  (letfn [(rember [a l]
            (cond (empty? l) '()
                  (test (first l) a) (rest l)
                  :else (cons (first l) (rember a (rest l)))) )]
    rember))

;;;
;;;    Name an "anonymous" function directly in Clojure...
;;;    
(defn rember-fn [test]
  (fn rember [a l]
    (cond (empty? l) '()
          (test (first l) a) (rest l)
          :else (cons (first l) (rember a (rest l)))) ))

(def rember-== (rember-fn ==))

(deftest test-rember-==
  (is (equal (rember-== 5 '(6 2 5 3)) '(6 2 3))))

(deftest test-rember-=
  (is (equal ((rember-fn =) 'tuna '(shrimp salad and tuna salad)) '(shrimp salad and salad))))

(defn insertl-f [test]
  (fn [old new l]
    (cond (empty? l) '()
          (test (first l) old) (cons new l)
          :else (cons (first l) ((insertl-f test) old new (rest l)))) ))

(defn insertl-f [test]
  (letfn [(insert [old new l]
            (cond (empty? l) '()
                  (test (first l) old) (cons new l)
                  :else (cons (first l) (insert old new (rest l)))) )]
    insert))

(defn insertl-f [test]
  (fn insert [old new l]
    (cond (empty? l) '()
          (test (first l) old) (cons new l)
          :else (cons (first l) (insert old new (rest l)))) ))

(def insertl-= (insertl-f =))

(deftest test-insertl-=
  (is (equal (insertl-= 'topping 'fudge '(ice cream with topping for dessert)) '(ice cream with fudge topping for dessert)))
  (is (equal (insertl-= 'salsa 'jalape??o '(tacos tamales and salsa)) '(tacos tamales and jalape??o salsa)))
  (is (equal (insertl-= 'f 'e '(a b c d f g d h)) '(a b c d e f g d h))))

(defn insertr-f [test]
  (fn [old new l]
    (cond (empty? l) '()
          (test (first l) old) (cons old (cons new (rest l)))
          :else (cons (first l) ((insertr-f test) old new (rest l)))) ))

(defn insertr-f [test]
  (letfn [(insert [old new l]
            (cond (empty? l) '()
                  (test (first l) old) (cons old (cons new (rest l)))
                  :else (cons (first l) (insert old new (rest l)))) )]
    insert))

(defn insertr-f [test]
  (fn insert [old new l]
    (cond (empty? l) '()
          (test (first l) old) (cons old (cons new (rest l)))
          :else (cons (first l) (insert old new (rest l)))) ))

(def insertr-= (insertr-f =))

(deftest test-insertr-=
  (is (equal (insertr-= 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert)))
  (is (equal (insertr-= 'and 'jalape??o '(tacos tamales and salsa)) '(tacos tamales and jalape??o salsa)))
  (is (equal (insertr-= 'd 'e '(a b c d f g d h)) '(a b c d e f g d h))))

(defn insert-g [test insert]
  (fn [old new l]
      (cond (empty? l) '()
            (test (first l) old) (insert old new l)
            :else (cons (first l) ((insert-g test insert) old new (rest l)))) ))

(defn insert-g [test seq]
  (letfn [(insert [old new l]
            (cond (empty? l) '()
                  (test (first l) old) (seq old new l)
                  :else (cons (first l) (insert old new (rest l)))) )]
    insert))

(defn insert-g [test seq]
  (fn insert [old new l]
    (cond (empty? l) '()
          (test (first l) old) (seq old new l)
          :else (cons (first l) (insert old new (rest l)))) ))

(def insertl-= (insert-g = (fn [old new l] (cons new l))))
(def insertr-= (insert-g = (fn [old new l] (cons old (cons new (rest l)))) ))

;;;
;;;    The book flips the order of NEW and OLD for SEQL and SEQR!
;;;    
(defn seql [a b l]
  "Insert B to the left of A at the head of L."
  (list* b a l))

(defn seqr [a b l]
  "Insert B to the right of A at the head of L."
  (list* a b l))

;;;
;;;    Compare skeleton/skeleton2 above.
;;;    
(defn insert-g [seq]
  (fn [old new l]
    (cond (empty? l) '()
          (= (first l) old) (seq old new (rest l))
          :else (cons (first l) ((insert-g seq) old new (rest l)))) ))

(defn insert-g [seq]
  (fn insert [old new l]
    (cond (empty? l) '()
          (= (first l) old) (seq old new (rest l))
          :else (cons (first l) (insert old new (rest l)))) ))

(def insertl (insert-g seql))
(def insertr (insert-g seqr))

(def insertl (insert-g #(list* %2 %1 %3)))
(def insertr (insert-g #(list* %1 %2 %3)))

(defn seqs [a b l]
  (cons b l))

(def subst (insert-g seqs))
(def subst (insert-g #(cons %2 %3)))

(defn seqrem [a b l]
  l)

;;;
;;;    Have to package the function! `rember` takes 2 args.
;;;    But `insert-g` is called at runtime!
;;;    
(def rember (fn [a l] ((insert-g #'seqrem) a nil l)))

(def rember (let [insert (insert-g #'seqrem)]
              (fn [a l] (insert a nil l))))

;;;
;;;    See `operation` above.
;;;    Book changes to Lisp syntax here: (+ 5 3)
;;;    
(defn atom-to-function [operator]
  (case operator
    + +
    * *
    ** expt))

(defn value [aexp]
  (cond (number? aexp) aexp
        :else (let [[operator op1 op2] aexp]
                ((atom-to-function operator) (value op1) (value op2)))) )

(deftest test-value
  (is (== (value '(+ 1 3)) 4))
  (is (== (value '(+ 1 (* 3 4))) 13))
  (is (== (value '(+ 1 (** 3 4))) 82)))

(defn set-f? [logical result]
  (fn [s1 s2]
      (cond (empty? s1) result
            :else (logical (member (first s1) s2) (set-f? logical result) s1 s2))))

(def subset? (set-f? (fn [bool continue s1 s2] (and bool (continue (rest s1) s2))) true))
(def intersect? (set-f? (fn [bool continue s1 s2] (or bool (continue (rest s1) s2))) false))

;;;
;;;    Book's approach...
;;;
(defn and* [p s1 s2]
  (and p (subset? (rest s1) s2))) ; Mutual recursion

(defn or* [p s1 s2]
  (or p (intersect? (rest s1) s2)))

(defn set-f? [logical result]
  (fn [s1 s2]
    (cond (empty? s1) result
          :else (logical (member (first s1) s2) s1 s2))))

(def subset? (set-f? and* true))
(def intersect? (set-f? or* false))

;;;
;;;    Y-combinator
;;;    
(defn multirember
  "Remove all instances of `a` from `l`."
  [a l]
  (cond (empty? l) '()
        (= (first l) a) (multirember a (rest l))
        :else (cons (first l) (multirember a (rest l)))) )

;; (multirember 'curry '(a b c curry e curry g curry)) => (a b c e g)
;; ((fn [l] (multirember 'curry l)) '(a b c curry e curry g curry)) => (a b c e g)
;; ((partial multirember 'curry) '(a b c curry e curry g curry)) => (a b c e g)
;; ((fn [l] ((fn [a] (multirember a l)) 'curry)) '(a b c curry e curry g curry)) => (a b c e g)

(defn mrember-curry [l]
  (multirember 'curry l))

(deftest test-mrember-curry
  (is (= (mrember-curry '(a b c curry e curry g curry)) '(a b c e g))))

(defn mrember-curry [l]
  (cond (empty? l) '()
        (= (first l) 'curry) (mrember-curry (rest l)) ; Recursive call to self
        :else (cons (first l) (mrember-curry (rest l)))) )

(defn curry-maker [future]
  (fn [l]
    (cond (empty? l) '()
          (= (first l) 'curry) ((curry-maker future) (rest l)) ; "Recursive" call to function that builds a function like mrember-curry
          :else (cons (first l) ((curry-maker future) (rest l)))) ))

;;;
;;;    Analogous...kinda (?)
;;;    mrember-curry:curry-maker::insertl:insert-g
;;;    
;;;    This is actually a bad example.
;;;      ??? `insertl` stops as soon as `old` is found. `multiinertl` is a better analog.
;;;      ??? The argument to `insert-g` is actually used. `future` is just a dummy value in `curry-maker`.
;;;    
;; (defn insertl [old new lat]
;;   (cond (empty? lat) '()
;;         (= (first lat) old) (cons new lat)
;;         :else (cons (first lat) (insertl old new (rest lat)))) )

;; (defn insert-g [seq]
;;   (fn [old new l]
;;     (cond (empty? l) '()
;;           (= (first l) old) (seq old new (rest l))
;;           :else (cons (first l) ((insert-g seq) old new (rest l)))) ))

;; (defn multiinsertl [old new lat]
;;   (cond (empty? lat) '()
;;         (= (first lat) old) (cons new (cons old (multiinsertl old new (rest lat))))
;;         :else (cons (first lat) (multiinsertl old new (rest lat)))) )

(def mrember-curry (curry-maker 0))
(def mrember-curry (curry-maker curry-maker))

(defn function-maker [future]
  (fn [l]
    (cond (empty? l) '()
          (= (first l) 'curry) ((future future) (rest l)) ; "Recursive" call to function passed as arg that builds function like mrember-curry (and remembers that arg)
          :else (cons (first l) ((future future) (rest l)))) ))

(def mrember-curry (function-maker function-maker))

(def mrember-curry ((fn [future]  ; Returns a function that takes a list as arg which is a closure over `future`.
                      (fn [l]
                        (cond (empty? l) '()
                              (= (first l) 'curry) ((future future) (rest l))
                              :else (cons (first l) ((future future) (rest l)))) ))
                    (fn [future]  ; Value of `future` captured above.
                      (fn [l]
                        (cond (empty? l) '()
                              (= (first l) 'curry) ((future future) (rest l))
                              :else (cons (first l) ((future future) (rest l)))) ))))

(((fn [future]
    (fn [l]
      (cond (empty? l) '()
            (= (first l) 'curry) ((future future) (rest l))
            :else (cons (first l) ((future future) (rest l)))) ))
  (fn [future]
    (fn [l]
      (cond (empty? l) '()
            (= (first l) 'curry) ((future future) (rest l))
            :else (cons (first l) ((future future) (rest l)))) )))
 '(a b c curry e curry g curry))

;;;
;;;    (f x) -> ((fn [x] (f x)) x)
;;;          -> (#(f %) x)
;;;    

(defn function-maker [future]
  (fn [l]
    (cond (empty? l) '()
          (= (first l) 'curry) ((fn [arg] ((future future) arg)) (rest l))
          :else (cons (first l) ((fn [arg] ((future future) arg)) (rest l)))) ))

;; ((function-maker function-maker) '(a b c curry e curry g curry)) => (a b c e g)

(defn function-maker [future]
  ((fn [rec]
     (fn [l]
       (cond (empty? l) '()
             (= (first l) 'curry) (rec (rest l))
             :else (cons (first l) (rec (rest l)))) ))
   (fn [arg] ((future future) arg))))

(defn helper [rec]
  (fn [l]
    (cond (empty? l) '()
          (= (first l) 'curry) (rec (rest l))
          :else (cons (first l) (rec (rest l)))) ))

(defn function-maker [future]
  (helper (fn [arg] ((future future) arg))))

(def mrember-curry ((fn [future]
                      (helper (fn [arg] ((future future) arg))))
                    (fn [future]
                      (helper (fn [arg] ((future future) arg)))) ))

(defn y [m]
  ((fn [future]
     (m (fn [arg] ((future future) arg))))
   (fn [future]
     (m (fn [arg] ((future future) arg)))) ))

(def mrember-curry (y helper))

(defn L [rec]
  (fn [l]
    (cond (empty? l) 0
          :else (inc (rec (rest l)))) ))

(def length (y L))

;; (length '(a b c)) => 3
;; (length '(a b)) => 2
;; (length '()) => 0

(def length (y (fn [rec]
                 (fn [l]
                   (cond (empty? l) 0
                         :else (inc (rec (rest l)))) ))))

(def length ((fn [m]
               ((fn [future]
                  (m (fn [arg] ((future future) arg))))
                (fn [future]
                  (m (fn [arg] ((future future) arg)))) ))
             (fn [rec]
               (fn [l]
                 (cond (empty? l) 0
                       :else (inc (rec (rest l)))) ))))

(((fn [m]
    ((fn [future]
       (m (fn [arg] ((future future) arg))))
     (fn [future]
       (m (fn [arg] ((future future) arg)))) ))
  (fn [rec]
    (fn [l]
      (cond (empty? l) 0
            :else (inc (rec (rest l)))) )))
 '(a b c))


;;;
;;;    Chapter 10
;;;

(ns interpreter
  (:use clojure.test
        [clojure.pprint :only (cl-format)]))

(def new-entry little/build)

(deftest test-new-entry
  (is (= (new-entry '(appetizer entr??e beverage) '(pat?? boeuf vin)) '((appetizer entr??e beverage) (pat?? boeuf vin))))
  (is (= (new-entry '(beverage dessert) '((food is) (number one with us))) '((beverage dessert) ((food is) (number one with us)))) ))

(defn lookup-in-entry-aux [name names values entry-f]
  (cond (empty? names) (entry-f name)
        (= (first names) name) (first values)
        :else (recur name (rest names) (rest values) entry-f)))

(defn lookup-in-entry [name entry entry-f]
  (lookup-in-entry-aux name (first entry) (second entry) entry-f))

(defn lookup-in-entry [name entry entry-f]
  (loop [names (first entry)
         values (second entry)]
    (cond (empty? names) (entry-f name)
          (= (first names) name) (first values)
          :else (recur (rest names) (rest values)))) )

(deftest test-lookup-in-entry
  (is (= (lookup-in-entry 'entr??e '((appetizer entr??e beverage) (food tastes good)) nil) 'tastes)))

(def extend-table clojure.core/cons)

(defn lookup-in-table [name table table-f]
  (cond (empty? table) (table-f name)
        :else (lookup-in-entry name (first table) #(lookup-in-table % (rest table) table-f))))

(deftest test-lookup-in-table
  (is (= (lookup-in-table 'entr??e '(((entr??e dessert) (spaghetti spumoni)) ((appetizer entr??e beverage) (food tastes good))) nil) 'spaghetti))
  (is (= (lookup-in-table 'beverage '(((entr??e dessert) (spaghetti spumoni)) ((appetizer entr??e beverage) (food tastes good))) nil) 'good)))

(declare *self-evaluating *identifier *quote *lambda *cond *application)


(defn atom-to-action [e]
  (cond (number? e) *self-evaluating
        (boolean? e) *self-evaluating
        (keyword? e) *self-evaluating
        :else *identifier))

(defn list-to-action [e]
  (cond (little/atom? (first e))
        (cond (= (first e) 'quote) *quote
              (= (first e) 'fn) *lambda
              (= (first e) 'cond) *cond
              :else *application)
        :else *application))

(defn expression-to-action [e]
  (cond (little/atom? e) (atom-to-action e)
        :else (list-to-action e)))

(defn meaning [e table]
  ((expression-to-action e) e table))

(defn value [e]
  (meaning e '()))

(defn *self-evaluating [e _]
  e)

(def text-of-quotation second)

(defn *quote [e _]
  (text-of-quotation e))

;; (defn *identifier [e table]
;;   (lookup-in-table e table (fn [name]
;;                              (case name
;;                                true true
;;                                  ((nil) nil)
;;                                  (otherwise (little:build primitive name)))) ))

(defn *identifier [e table]
  (lookup-in-table e table #(little/build :primitive %)))

;;;
;;;    Builds a closure: ordered triple of environment, params, body
;;;    ((lambda (x y) (cons x y)) 1 '(2)) => (:non-primitive (((x y) (1 (2))) (x y) (cons x y)))
;;;    
(defn *lambda [e table]
  (little/build :non-primitive (clojure.core/cons table (rest e))))

(def table-of  first)
(def formals-of second)
(def body-of (comp second rest))


(deftest test-meaning
  (is (= (meaning 'entr??e '(((entr??e dessert) (spaghetti spumoni)) ((appetizer entr??e beverage) (food tastes good)))) 'spaghetti))
  (is (= (meaning '(fn [x] (cons x y)) '(((y z) ((8) 9)))) '(:non-primitive ((((y z) ((8) 9))) (x) (cons x y)))) ))

(def question-of first)
(def answer-of second)

(defn evcon
  "Evaluate COND form."
  [lines table]
  (cond (meaning (question-of (first lines)) table) (meaning (answer-of (first lines)) table)
        :else (evcon (rest lines) table)))

(def cond-lines rest)

(defn *cond [e table]
  (evcon (cond-lines e) table))


;(*cond '(cond (coffee klatsch) (t party)) '(((coffee) (t)) ((klatsch party) (5 (6))))) => 5

(defn evlis [args table]
  (cond (empty? args) '()
        :else (clojure.core/cons (meaning (first args) table) (evlis (rest args) table))))

(deftest test-evlis
  (is (= (evlis '(x y) '(((x y) (1 (2)))) ) '(1 (2))))
  (is (= (evlis '(z x) '(((x y) ((a b c) (d e f))) ((u v w) (1 2 3)) ((x y z) (4 5 6)))) '(6 (a b c)))) )

(def function-of first)
(def arguments-of rest)

(defn primitive? [f]
  (= (first f) :primitive))

(defn non-primitive? [f]
  (= (first f) :non-primitive))

(defn apply-primitive [name args]
  (case name
    first (first (first args)) ; Self-reference
    rest (rest (first args))
    cons (clojure.core/cons (first args) (second args))
    = (= (first args) (second args))
    atom? (little/atom? (first args))
    not (not (first args))
    empty? (empty? (first args))
    number? (number? (first args))
    zero? (zero? (first args))
    inc (inc (first args))
    dec (dec (first args))))

(defn apply-closure [closure args]
  (meaning (body-of closure) (extend-table (new-entry (formals-of closure) args) (table-of closure))))

(deftest test-apply-closure
  (is (= (apply-closure '((((u v w) (1 2 3)) ((x y z) (4 5 6))) (x y) (cons z x)) '((a b c) (d e f))) '(6 a b c))))

(defn apply [f args]
  (cond (primitive? f) (apply-primitive (second f) args)
        (non-primitive? f) (apply-closure (second f) args)))

(defn *application [e table]
  (apply (meaning (function-of e) table)
         (evlis (arguments-of e) table)))

;;;
;;;    Radical refactoring
;;;
(defn cons [u v]
  (fn [b]
    (cond b u
          :otherwise v)))

(defn car [l]
  (l true))

(defn cdr [l]
  (l false))

(def lunch (cons 'apple '()))

;; (car lunch) => apple
;; (cdr lunch) => ()
;; (cons 'banana lunch) => #object[interpreter$cons$fn__203888 0x6a52e1ec "interpreter$cons$fn__203888@6a52e1ec"]
;; (car *1) => banana
;; (cdr *2) => #object[interpreter$cons$fn__203888 0x2d8ffa2c "interpreter$cons$fn__203888@2d8ffa2c"]
;; (car (cdr *3)) => apple

;; (defun apply-primitive (name args)
;;   (ecase name
;;     (eql (eql (first args) (second args)))
;;     (atom (atom (first args)))) )

    ;; (not (not (first args)))
    ;; (null (null (first args)))
    ;; (numberp (numberp (first args)))
    ;; (zerop (zerop (first args)))
    ;; (1+ (1+ (first args)))
    ;; (1- (1- (first args)))) )

;;;
;;;    PERSON as closure rather than structure/object...
;;;    - Parameters as fields
;;;    - All of this could be bundled into a macro to define accessors.
;;;    
(defn defperson [name age]
  (fn [command]
    (case command
      name name
      age age)))

(defn age [person]
  (person 'age))

(defn name [person]
  (person 'name))

;; (defperson 'bob 42) => #object[interpreter$defperson$fn__205395 0x6c1c123e "interpreter$defperson$fn__205395@6c1c123e"]
;; (name *1) => bob
;; (age *2) => 42
