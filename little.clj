;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               little.clj
;;;;
;;;;   Started:            Sun Mar 22 04:20:38 2009
;;;;   Modifications:
;;;;
;;;;   Purpose: Exercises from The Little Lisper
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
  (:use)
  (:import))

(declare null?)

;;;
;;;    Chapter 1 - Necessary primitives defined
;;;

;;;
;;;    This definition is from The Little Schemer pg. xii
;;;    But semantics are different. Namely, (atom? '()) => false
;;;
;; (defn atom? [obj]
;;   (not (list? obj)))

(defn atom? [obj]
  (or (not (coll? obj))
      (= obj '())))

;; (defn atom? [obj]
;;   (or (not (coll? obj))
;;       (null? obj)))

;; (defn atom? [obj]
;;   (or (symbol? obj)
;;       (number? obj)
;;       (string? obj)
;;       (keyword? obj)
;;       (= (class obj) java.lang.Character)
;;       (= obj '())))

(defn test-atom [f]
  (every? true? (map f ['pung 8 9.2 1/2 "pung" :pung \p '()])))

(defn car [obj]
  (if (and (list? obj) (seq obj))
    (first obj)
    (throw (Exception. (format "Don't know how to find car of %s"
                               (pr-str obj)))) ))

(defn cdr [obj]
  (if (and (list? obj) (seq obj))
    (rest obj)
    (throw (Exception. (format "Don't know how to find cdr of %s"
                               (pr-str obj)))) ))

(defn null? [obj]
  (if (list? obj)
    (= obj '())
    (throw (Exception. (format "Don't know how to evaluate null? of %s"
                               (pr-str obj)))) ))

(defn eq? [o1 o2]
  (if (and (atom? o1) (atom? o2))
    (identical? o1 o2)
    (throw (Exception. (format "Don't know whether %s and %s are eq?"
                               (pr-str o1)
                               (pr-str o2)))) ))

;;;
;;;    Chapter 2
;;;

(defn lat? [obj]
  (cond (null? obj) true
        (atom? (car obj)) (recur (cdr obj))
        :else false))

;; (defn lat? [l]
;;   (every? atom? l))
