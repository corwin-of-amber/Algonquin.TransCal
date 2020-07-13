;(set-logic ALL_SUPPORTED)

(declare-datatypes () ((Nat (succ (pred Nat)) (zero))))

(declare-fun plus (Nat Nat) Nat)
(assert (forall ((n Nat)) (= (plus zero n) n)))
(assert (forall ((n Nat) (m Nat)) (= (plus (succ n) m) (succ (plus n m)))))

(declare-fun mult (Nat Nat) Nat)
(assert (forall ((n Nat)) (= (mult zero n) zero)))
(assert (forall ((n Nat) (m Nat)) (= (mult (succ n) m) (plus (mult n m) m))))

(declare-fun tri (Nat) Nat)
(assert (= (tri zero) zero))
(assert (forall ((n Nat)) (= (tri (succ n)) (plus (tri n) (succ n)))))

(declare-fun cubes (Nat) Nat)
(assert (= (cubes zero) zero))
(assert (forall ((n Nat)) (= (cubes (succ n)) (plus (cubes n) (mult (succ n) (mult (succ n) (succ n)))))))

; mapping to integers
(declare-fun nat-to-int (Nat) Int)
; it is an injection to positive integers
(assert (forall ((x Nat)) (>= (nat-to-int x) 0)))
(assert (forall ((x Nat) (y Nat)) (=> (= (nat-to-int x) (nat-to-int y)) (= x y))))
; mapping for functions
(assert (= (nat-to-int zero) 0))
(assert (forall ((x Nat)) (= (nat-to-int (succ x)) (+ 1 (nat-to-int x)))))
(assert (forall ((x Nat) (y Nat)) (= (nat-to-int (plus x y)) (+ (nat-to-int x) (nat-to-int y)))))
;(assert (forall ((x Nat) (y Nat)) (= (nat-to-int (mult x y)) (* (nat-to-int x) (nat-to-int y)))))


; proven
(assert 
(forall ((n Nat) (m Nat)) (= (plus n m) (plus m n)))  ; G1 
)
(assert 
(forall ((x Nat) (y Nat) (z Nat)) (= (plus x (plus y z)) (plus (plus y x) z)))  ; G2 
)
(assert 
(forall ((n Nat) (m Nat)) (= (mult n m) (mult m n)))  ; G3 
)
(assert 
(forall ((x Nat) (y Nat) (z Nat)) (= (mult x (mult y z)) (mult (mult y x) z)))  ; G4 
)

; conjecture
(assert (not 
(forall ((x Nat) (y Nat)) (= (mult x (plus y y)) (mult y (plus x x))))  ; G5 
))
(check-sat)
