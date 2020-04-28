Declare ML Module "thesy".

Inductive TheSy (E : Set) := A : TheSy E | B : E -> nat -> TheSy E.

Definition f (e : TheSy nat) :=
    match e with
    | A _ => 0
    | B _ x y => x + y
    end.

(*CallC.*)
CallC "Match.json".

(*Inductive bool := t | f.
Definition gggg := t.
Lemma foo : True.
Proof.
hello. now auto.
Qed.*)

