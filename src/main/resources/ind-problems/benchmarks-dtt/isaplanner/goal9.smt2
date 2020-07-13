(declare-datatypes () ((Lst (cons (head Int) (tail Lst)) (nil))))
(declare-datatypes () ((Tree (node (data Int) (left Tree) (right Tree)) (leaf))))
(declare-datatypes () ((Pair (mkpair (first Int) (second Int)))
                       (ZLst (zcons (zhead Pair) (ztail ZLst)) (znil))))
(declare-fun P (Int) Bool)
(declare-fun f (Int) Int)
(declare-fun less (Int Int) Bool)
(declare-fun plus (Int Int) Int)
(declare-fun minus (Int Int) Int)
(declare-fun mult (Int Int) Int)
(declare-fun nmax (Int Int) Int)
(declare-fun nmin (Int Int) Int)
(declare-fun append (Lst Lst) Lst)
(declare-fun len (Lst) Int)
(declare-fun drop (Int Lst) Lst)
(declare-fun take (Int Lst) Lst)
(declare-fun count (Int Lst) Int)
(declare-fun last (Lst) Int)
(declare-fun butlast (Lst) Lst)
(declare-fun mem (Int Lst) Bool)
(declare-fun delete (Int Lst) Lst)
(declare-fun rev (Lst) Lst)
(declare-fun lmap (Lst) Lst)
(declare-fun filter (Lst) Lst)
(declare-fun dropWhile (Lst) Lst)
(declare-fun takeWhile (Lst) Lst)
(declare-fun ins1 (Int Lst) Lst)
(declare-fun insort (Int Lst) Lst)
(declare-fun sorted (Lst) Bool)
(declare-fun sort (Lst) Lst)
(declare-fun zip (Lst Lst) ZLst)
(declare-fun zappend (ZLst ZLst) ZLst)
(declare-fun zdrop (Int ZLst) ZLst)
(declare-fun ztake (Int ZLst) ZLst)
(declare-fun zrev (ZLst) ZLst)
(declare-fun mirror (Tree) Tree)
(declare-fun height (Tree) Int)
(define-fun leq ((x Int) (y Int)) Bool (or (= x y) (less x y)))
(assert (forall ((n Int)) (=> (>= n 0) (= (plus 0 n) n))))
(assert (forall ((n Int) (m Int)) (=> (and (>= n 0) (>= m 0)) (= (plus (+ 1 n) m) (+ 1 (plus n m))))))
(assert (forall ((n Int) (m Int)) (=> (and (>= n 0) (>= m 0)) (= (plus n m) (+ n m)))))
(assert (forall ((n Int)) (=> (>= n 0) (= (minus 0 n) 0))))
(assert (forall ((n Int)) (=> (>= n 0) (= (minus n 0) n))))
(assert (forall ((n Int) (m Int)) (=> (and (>= n 0) (>= m 0)) (= (minus (+ 1 n) (+ 1 m)) (minus n m)))))
(assert (forall ((n Int) (m Int)) (=> (and (>= n 0) (>= m 0)) (= (minus n m) (ite (< n m) 0 (- n m))))))
(assert (not 
(forall ((i Int) (j Int) (k Int)) (=> (and (>= i 0) (>= j 0) (>= k 0)) (= (minus (minus i j) k) (minus i (plus j k))))) ; G9 
))
(check-sat)
