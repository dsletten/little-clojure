;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               little.clj
;;;;
;;;;   Started:            Sun Oct  3 02:18:21 2010
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
;;;;   Notes: Exercises from The Little Lisper
;;;;
;;;;   Be careful of case-sensitivity when testing with symbols.
;;;;   Can't just copy and past tests from Common Lisp!
;;;;

(ns little)
;  (:use)
;  (:import))

;;;
;;;    Returns true for '()
;;;    
(defn atom? [obj]
  (not (seq? obj)))

;;;
;;;    Little Schemer
;;;
;; (defn atom? [obj]
;;   (not (or (seq? obj)
;;            (empty? obj))))

(defn eq? [a b]
  (and (atom? a)
       (atom? b)
       (= a b)))

;;;
;;;    Ch. 2
;;;
(defn lat? [l]
  (cond (empty? l) true
        (atom? (first l)) (lat? (rest l))
        :else false))

;;;
;;;    Defeats the point of the book...
;;;    
;; (defn lat? [l]
;;   (every? atom? l))

(defn lat? [l]
  (loop [list l]
    (cond (empty? list) true
          (atom? (first list)) (recur (rest list))
          :else false)))

;; (lat? '(is this not pung?))
;; (lat? '(is (this) not pung?))

;;;
;;;    Book uses strange definition here.
;;;    
(defn member? [a lat]
  (cond (empty? lat) false
        :else (or (= (first lat) a)
                  (member? a (rest lat)))) )

(defn member? [obj l]
  (cond (empty? l) false
        (= (first l) obj) l
        :else (member? obj (rest l))))

(defn member? [obj l]
  (loop [list l]
    (cond (empty? list) false
          (= (first list) obj) list
          :else (recur (rest list)))) )

(member? 'tea '(coffee tea or milk))
(member? 'poached '(fried eggs and scrambled eggs))
(member? 'meat '(mashed potatoes and meat gravy))
(member? 'liver '(bagels and lox))

;;;
;;;    () vs. false for failure.
;;;    
;; (defn member? [obj l]
;;   (drop-while (fn [elt] (not= elt obj)) l))

;; (defn member? [obj l]
;;   (let [result (drop-while (fn [elt] (not= elt obj)) l)]
;;     (if (seq result)
;;       result
;;       false)))

(defn member? [obj l]
  (let [result (drop-while (fn [elt] (not= elt obj)) l)]
    (if (empty? result)
      false
      result)))

;;;
;;;    Ch. 3
;;;
(defn rember [a lat]
  (cond (empty? lat) '()
        (= (first lat) a) (rest lat)
        :else (cons (first lat) (rember a (rest lat)))) )

(rember 'mint '(lamb chops and mint jelly))
(rember 'mint '(lamb chops and mint flavored mint jelly))
(rember 'toast '(bacon lettuce and tomato))
(rember 'cup '(coffee cup tea cup and hick cup))

(defn rember [a lat]
  (loop [l lat
         result '()]
    (cond (empty? l) (reverse result)
          (= (first l) a) (concat (reverse result) (rest l))
          :else (recur (rest l) (cons (first l) result)))) )

(def rember (fn [a lat]
              (cond (empty? lat) '()
                    (= (first lat) a) (rest lat)
                    :else (cons (first lat) (rember a (rest lat)))) ))

(defn firsts [lol]
  (if (empty? lol)
    '()
    (cons (first (first lol)) (firsts (rest lol)))) )

;; (defn firsts [lol]
;;   (map first lol))

(defn test-firsts []
  (and
   (= (firsts '((a b) (c d) (e f))) '(a c e))
   (= (firsts '()) '())
   (= (firsts '((five plums) (four) (eleven green oranges)))
      '(five four eleven))))

(defn insert-r [old new lat]
  (cond (empty? lat) '()
        (= (first lat) old) (cons (first lat) (cons new (rest lat)))
        :else (cons (first lat) (insert-r old new (rest lat)))) )

(defn test-insert-r []
  (and
   (= (insert-r 'fudge 'topping '(ice cream with fudge for dessert))
      '(ice cream with fudge topping for dessert))
   (= (insert-r 'and 'jalapeno '(tacos tamales and salsa))
      '(tacos tamales and jalapeno salsa))
   (= (insert-r 'd 'e '(a b c d f g d h))
      '(a b c d e f g d h))))

