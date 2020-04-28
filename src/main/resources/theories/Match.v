Declare ML Module "thesy".

Inductive TheSy (E : Set) := A : TheSy E | B : E -> nat -> TheSy E -> TheSy E | C : TheSy E.

Definition f (e : TheSy nat) :=
    match e with
    | A _ => 0
    | C _ => 1
    | B _ x _ (B _ y _ _) => x + y
    | B _ x y _ => x + y
    end.

(*CallC.*)
CallC "Match.json".

(*Inductive bool := t | f.
Definition gggg := t.
Lemma foo : True.
Proof.
hello. now auto.
Qed.*)
