Declare ML Module "thesy".

Inductive mylist (A: Set) := mynil : mylist A | mycons : A -> mylist A -> mylist A.

Fixpoint f a (e : mylist a) :=
    match e with
    | mynil _ => 0
    | mycons _ x xs => 1 + (f a xs)
    end.

(*CallC.*)
CallC "Recursion.json".

(*Inductive bool := t | f.
Definition gggg := t.
Lemma foo : True.
Proof.
hello. now auto.
Qed.*)

