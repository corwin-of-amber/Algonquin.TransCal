Declare ML Module "thesy". 

Definition f (e : list nat) :=
    match e with
    | nil => 0
    | cons y (cons x xs)=> x + y
    | cons x xs => x
    end.

Definition g (e : list nat) :=
    match e return list nat with
    | nil => nil
    | cons y (cons x xs) as t => cons x t
    | cons x xs => xs
    end.

Definition h (e : list nat) :=
    match e return list nat with
    | nil => nil
    | cons x xs => nil
    end.


(*CallC.*)
CallC "Match.json".

(*Inductive bool := t | f.
Definition gggg := t.
Lemma foo : True.
Proof.
hello. now auto.
Qed.*)

