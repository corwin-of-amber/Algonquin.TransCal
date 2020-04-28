Declare ML Module "thesy".


Lemma test : True \/ False -> True.
Proof.
intro;
constructor.
Qed.

Definition a_number := 2.
Definition b_num x := x + x + a_number.

Inductive TheSy (E : Set) := A : TheSy E | B : E -> nat -> TheSy E.

Definition f (e : TheSy nat) :=
    match e with
    | A _ => 0
    | B _ x y => x + y + a_number
    end.

(*CallC.*)
CallC.

(*Inductive bool := t | f.
Definition gggg := t.
Lemma foo : True.
Proof.
hello. now auto.
Qed.*)

