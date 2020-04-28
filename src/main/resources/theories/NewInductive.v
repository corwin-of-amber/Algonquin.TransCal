Declare ML Module "thesy".

Inductive TheSy (E : Set) := A : TheSy E | B : E -> TheSy E -> TheSy E.
Definition foo (t: TheSy nat) := B nat 1 t.

(*CallC.*)
CallC "NewInductive.json".

(*Inductive bool := t | f.
Definition gggg := t.
Lemma foo : True.
Proof.
hello. now auto.
Qed.*)

