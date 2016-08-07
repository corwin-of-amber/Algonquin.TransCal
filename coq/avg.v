Require Import List.

Check nil.

Require Coq.Reals.Reals.

Import Coq.Reals.Rdefinitions.
Import Coq.Reals.Raxioms.

Open Scope list_scope.
Open Scope R_scope.

Check nil.

Check Coq.Reals.Rdefinitions.R.


Ltac dismiss := eexists; reflexivity.  (* used to finish a goal {y | y = ...} in the obvious way *)


Definition uncurry2 {A B C} (f : A -> B -> C) ab := f (fst ab) (snd ab).
    
Lemma uncurrying2 {A B C} (f : A -> B -> C) a b : f a b = uncurry2 f (a, b).
Admitted.

(* this is useless in its current form.
 * The idea is that a program of the form {y | y = f a} can be
 * composed by a program computing {y | y = a} followed by an application
 * of f; the program {y | y = a} can then be further refined.
 * But this one does not work because the fourth argument (a) is
 * not eliminated during extraction.
 *
Lemma seq {A B} (f : A -> B) a : {y | y = a} -> {y | y = f a}.
Admitted. *)



Notation Real := Coq.Reals.Rdefinitions.R.

Section Average.

  Fixpoint sum (l : list Real) : Real :=
    match l with
    | nil => 0
    | x::xs => x + sum xs
    end.

  Definition avg l := sum l / INR (length l).


  Lemma INR_S n : INR (S n) = 1 + INR n.
  Proof.
    simpl.
    destruct n.
    - simpl. rewrite RIneq.Rplus_0_r. reflexivity.
    - rewrite Rplus_comm. reflexivity.
  Qed.

    
  Arguments INR n : simpl nomatch.

  Lemma avg' l : {y | y = avg l}.
  Proof.
    unfold avg.
    (*rewrite (uncurrying2 Rdiv).
    apply seq.*)
    case l as [|x xs]; simpl.
    - dismiss.
    - rewrite INR_S. generalize x 1 xs.  (* "1" is called 'r' from now on *)
      fix aux 3.
      destruct xs0 as [|x' xs']; simpl.
      + rewrite !RIneq.Rplus_0_r. dismiss.
      + rewrite INR_S. rewrite <- !Rplus_assoc. apply aux.
  Defined.

  Compute (fun l => proj1_sig (avg' l)).
  
  Check exist.
  Search exist.
  Definition sigval {A} {P : A -> Prop} (x: {x | P x}) :=
    match x with exist _ v _ => v end.

  Lemma sigval_P {A} {P : A -> Prop} (x: {x | P x}) : P (sigval x).
    destruct x.
    simpl.
    assumption.
  Qed.

  Lemma avg'avg l : avg l = sigval (avg' l).
    destruct (avg' l).  (* Would be nice to use sigval_P? *)
    simpl.
    symmetry; assumption.
  Qed.

  Fixpoint avg'0 l :=
    match l with
    | nil => 0 / 0
    | x :: xs => aux x 1 xs
    end
  with aux x j xs :=
    match xs with
    | nil => x / j
    | x' :: xs' => aux (x + x') (j + 1) xs'
    end.

End Average.


Section Stddev.

  Definition sqr x := x * x.

  Definition variance l := avg (map (fun x => sqr (x - avg l)) l).

  Print avg'.
  
  Arguments INR n : simpl nomatch.

  Lemma variance' l : {y | y = variance l}.
  Proof.
    unfold variance.
    generalize (avg l); intro avgl. (* generalize;intro  =  let insertion *)

    (* Now I am basically replicating avg' *)
    case l as [|x xs]; unfold avg; simpl.
    - dismiss.
    - rewrite INR_S. generalize (sqr (x - avgl)) 1 xs.
      fix vaux 3.
      intros. case xs0 as [|x' xs']; simpl.
      + rewrite !RIneq.Rplus_0_r. dismiss.
      + rewrite INR_S. rewrite <- !Rplus_assoc. apply vaux.
  Qed.

  Lemma variance'' l : {y | y = avg'0 (map (fun x => sqr (x - avg l)) l)}.
  Proof.
    case l at 2; simpl.
    - dismiss.
    - generalize (sqr (r - avg l)), 1, l0 as l1; fix vaux 3.
      destruct l1; simpl.
      + dismiss.
      + apply vaux. 
  Defined.
  
End Stddev.


Extraction variance'.
Extraction variance''.