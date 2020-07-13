(declare-datatypes () ((Nat (succ (pred Nat)) (zero))))
(declare-datatypes () ((Lst (cons (head Nat) (tail Lst)) (nil))))
(declare-datatypes () ((Tree (node (data Nat) (left Tree) (right Tree)) (leaf))))
(declare-datatypes () ((Pair (mkpair (first Nat) (second Nat)))
                       (ZLst (zcons (zhead Pair) (ztail ZLst)) (znil))))
(declare-fun P (Nat) Bool)
(declare-fun f (Nat) Nat)
(declare-fun less (Nat Nat) Bool)
(declare-fun plus (Nat Nat) Nat)
(declare-fun minus (Nat Nat) Nat)
(declare-fun mult (Nat Nat) Nat)
(declare-fun nmax (Nat Nat) Nat)
(declare-fun nmin (Nat Nat) Nat)
(declare-fun nat-to-int (Nat) Int)
(declare-fun append (Lst Lst) Lst)
(declare-fun len (Lst) Nat)
(declare-fun drop (Nat Lst) Lst)
(declare-fun take (Nat Lst) Lst)
(declare-fun count (Nat Lst) Nat)
(declare-fun last (Lst) Nat)
(declare-fun butlast (Lst) Lst)
(declare-fun mem (Nat Lst) Bool)
(declare-fun delete (Nat Lst) Lst)
(declare-fun rev (Lst) Lst)
(declare-fun lmap (Lst) Lst)
(declare-fun filter (Lst) Lst)
(declare-fun dropWhile (Lst) Lst)
(declare-fun takeWhile (Lst) Lst)
(declare-fun ins1 (Nat Lst) Lst)
(declare-fun insort (Nat Lst) Lst)
(declare-fun sorted (Lst) Bool)
(declare-fun sort (Lst) Lst)
(declare-fun zip (Lst Lst) ZLst)
(declare-fun zappend (ZLst ZLst) ZLst)
(declare-fun zdrop (Nat ZLst) ZLst)
(declare-fun ztake (Nat ZLst) ZLst)
(declare-fun zrev (ZLst) ZLst)
(declare-fun mirror (Tree) Tree)
(declare-fun height (Tree) Nat)
(define-fun leq ((x Nat) (y Nat)) Bool (or (= x y) (less x y)))
(assert (forall ((x Nat)) (>= (nat-to-int x) 0)))
(assert (forall ((x Nat) (y Nat)) (=> (= (nat-to-int x) (nat-to-int y)) (= x y))))
(assert (= (nat-to-int zero) 0))
(assert (forall ((x Nat)) (= (nat-to-int (succ x)) (+ 1 (nat-to-int x)))))
(assert (forall ((x Nat)) (= (drop x nil) nil)))
(assert (forall ((x Lst)) (= (drop zero x) x)))
(assert (forall ((x Nat) (y Nat) (z Lst)) (= (drop (succ x) (cons y z)) (drop x z))))
(assert (forall ((x Lst)) (= (zip nil x) znil)))
(assert (forall ((x Lst)) (= (zip x nil) znil)))
(assert (forall ((x Nat) (y Lst) (z Nat) (w Lst)) (= (zip (cons x y) (cons z w)) (zcons (mkpair x z) (zip y w)))))
(assert (forall ((x Nat)) (= (zdrop x znil) znil)))
(assert (forall ((x ZLst)) (= (zdrop zero x) x)))
(assert (forall ((x Nat) (y Pair) (z ZLst)) (= (zdrop (succ x) (zcons y z)) (zdrop x z))))
(assert (not 
(forall ((n Nat) (xs Lst) (ys Lst)) (= (zdrop n (zip xs ys)) (zip (drop n xs) (drop n ys)))) ; G59 
))
(check-sat)
