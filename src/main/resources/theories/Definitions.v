Declare ML Module "thesy".

Definition const_number := 2.
Definition num_from_const x := x + const_number.
Definition foo a (l: list a) := l.

(*CallC.*)
CallC "Definitions.json".

(*Inductive bool := t | f.
Definition gggg := t.
Lemma foo : True.
Proof.
hello. now auto.
Qed.*)

