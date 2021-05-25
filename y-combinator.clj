;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               y-combinator.clj
;;;;
;;;;   Started:            Sun Mar 29 20:40:27 2009
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

(ns y-combinator
  (:use)
  (:import))

(defn Y [m]
  ((fn [future]
     (m (fn [arg]
          ((future future) arg))))
   (fn [future]
     (m (fn [arg]
          ((future future) arg)))) ))

(defn factorial [n]
  (if (zero? n)
    1
    (* n (factorial (dec n)))) )

((fn [n] (if (zero? n) 1 (* n (?? (dec n))))) 6)

(defn Y2 [m]
  ((fn [future]
     (m (fn [& args]
          (apply (future future) args))))
   (fn [future]
     (m (fn [& args]
          (apply (future future) args)))) ))

;;;
;;;    FACT
;;;    
((Y (fn [rec]
      (fn [n]
        (if (zero? n)
          1
          (* n (rec (dec n)))) ))) 6)

;;;
;;;    FIBONACCI
;;;    
((Y (fn [rec]
      (fn [n]
        (cond (= n 0) 0
              (= n 1) 1
              :else (+ (rec (- n 1)) (rec (- n 2)))) ))) 10)

;;;
;;;    LENGTH
;;;    
((Y (fn [rec]
      (fn [l]
        (if (empty? l)
          0
          (inc (rec (rest l)))) ))) '(a b c d e))

;;;
;;;    REVERSE
;;;    
((Y (fn [rec]
      (fn [l]
        (cond (empty? l) '()
              (empty? (rest l)) (list (first l))
              :else (cons (first (rec (rest l)))
                          (rec (cons (first l)
                                     (rec (rest (rec (rest l)))) )))) )))
 '(a b c d e))

;;;
;;;    REMOVE
;;;    
((Y2 (fn [rec]
       (fn [obj l]
         (cond (empty? l) '()
               (= (first l) obj) (rec obj (rest l))
               :else (cons (first l) (rec obj (rest l)))) )))
 'pung
 '(pung foo bar baz pung baz bar pung foo))

;;;
;;;    REPLACE
;;;    
((Y2 (fn [rec]
       (fn [new old l]
         (cond (empty? l) '()
               (= (first l) old) (cons new (rec new old (rest l)))
               :else (cons (first l) (rec new old (rest l)))) )))
 'pung
 'foo
 '(pung foo bar baz pung bar foo))

;;;
;;;    SUBST
;;;    
((Y2 (fn [rec]
       (fn [new old obj]
         (cond (= obj old) new
               (and (coll? obj) (seq obj)) (cons (rec new old (first obj))
                                                 (rec new old (rest obj)))
               :else obj))))
 'a
 'b
 '(a ((b) c (a b c)) d (a b)))
         
;;;
;;;    Simple MAP over single seq
;;;
((Y2 (fn [rec]
       (fn [f l]
         (if (empty? l)
           '()
           (cons (f (first l)) (rec f (rest l)))) )))
 inc
 (range 10))

((Y2 (fn [rec]
       (fn [f l]
         (if (empty? l)
           '()
           (cons (f (first l)) (rec f (rest l)))) )))
 #(.toUpperCase %)
 '("Is" "this" "not" "pung?"))

;;;
;;;    Map over multiple seqs
;;;    
;; ((Y2 (fn [rec]
;;        (fn [f & ls]
;;          (if (some empty? ls)
;;            '()
;;            (cons () 

;;;
;;;    REDUCE
;;;    
((Y2 (fn [rec]
       (fn [f start l]
         (if (empty? l)
           start
           (f (first l) (rec f start (rest l)))) )))
 +
 0
 [1 2 3 4 5])

((Y2 (fn [rec]
       (fn [f start l]
         (if (empty? l)
           start
           (f (first l) (rec f start (rest l)))) )))
 *
 1
 [1 2 3 4 5 6])

;;;
;;;    Anonymous Y!!
;;;
;;;    LENGTH
;;;    
(((fn [m]
    ((fn [future]
       (m (fn [arg]
            ((future future) arg))))
     (fn [future]
       (m (fn [arg]
            ((future future) arg)))) ))
  (fn [rec]
    (fn [l]
      (if (empty? l)
        0
        (inc (rec (rest l)))) )))
 '(a b c d e)) 

;;;
;;;    REVERSE
;;;    
(((fn [m]
    ((fn [future]
       (m (fn [arg]
            ((future future) arg))))
     (fn [future]
       (m (fn [arg]
            ((future future) arg)))) ))
  (fn [rec]
    (fn [l]
      (cond (empty? l) '()
            (empty? (rest l)) (list (first l))
            :else (cons (first (rec (rest l)))
                        (rec (cons (first l)
                                   (rec (rest (rec (rest l)))) )))) )))
 '(a b c d e))
