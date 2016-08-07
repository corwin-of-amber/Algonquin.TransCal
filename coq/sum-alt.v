
Require Import List.
Require Import Coq.ZArith.ZArith.

Require Import Coq.Setoids.Setoid.

Open Scope list_scope.

(*------------------------------------------------------------------------*)

Section Sum.

  Fixpoint sum (l: list nat) :=
    match l with
    | nil => 0
    | x::xs => x + sum xs
    end.

  (*
  Lemma sum_concat (l1 l2 : list nat) : sum (l1 ++ l2) = sum l1 + sum l2.
    induction l1.
    - simpl. trivial.
    - simpl.
      rewrite IHl1.
      rewrite Plus.plus_assoc.
      trivial.
  Qed.
   *)
  
  Lemma sum' (l: list nat) : {y | y = sum l}.
  Proof.
    case l as [|x xs]; simpl.
    - eexists; reflexivity.                                 (* l = nil => y = 0 *)

    - generalize x; induction xs as [|x' xs'].              (* l = x :: xs => y = f x xs *)

      + eexists; simpl; rewrite <- plus_n_O. reflexivity.    (* xs = nil => y = x0 *)
      + intros.                                             (* xs = x' :: xs' *)
        edestruct IHxs' as [r R].
        eexists.
        simpl; rewrite Plus.plus_assoc.
        rewrite <- R.
        reflexivity.                                        (* => y = f (x0 + x') xs' *)
  Defined.


  (* Now using fix *)
  Lemma sum'' (l: list nat) : {y | y = sum l}.
  Proof.
    case l as [|x xs]; simpl.
    - eexists; reflexivity.
    - generalize x xs.
      fix H0 2.
      destruct xs0 as [|x' xs'].
      + eexists; simpl; rewrite <- plus_n_O. reflexivity.
      + simpl. rewrite Plus.plus_assoc. apply H0.
  Defined.

    
End Sum.

(* proof term *)
Print sum'.
(* holy shit! *)
Extraction sum'.
Extraction sum''.



Fixpoint parity (n: nat) : bool :=
  match n with
  | O => false
  | S n => negb (parity n)
  end.

Lemma parity' : forall n, {y | parity n = y}.
  fix H0 1.
  case n as [|n0].
  - simpl; eexists; reflexivity.
  - case n0 as [|n1].
    + simpl; eexists; reflexivity.
    + simpl. rewrite Bool.negb_involutive.
      apply H0.
Defined.

Extraction parity'.

(* Requires strong induction; most likely not what we want *)
(*
Lemma parity'_strong_induction (n: nat) : {y | parity n = y}.
  induction n using strong_induction_type.
  destruct n.
  - simpl; eexists; reflexivity.
  - destruct n.
    + simpl; eexists; reflexivity.
    + simpl. rewrite Bool.negb_involutive.
      apply H.
      constructor. reflexivity.
Qed.

Extraction parity'_strong_induction.
Extraction strong_induction_type'.
 *)


(*
 * These don't really work.

Lemma parity'' (n: nat) : {y | parity n = y /\ parity (S n) = negb y}.
Proof.
  induction n as [|n0].
  - simpl. eexists. split; reflexivity.
  - destruct n0 as [|n1].
    + simpl. eexists; split; reflexivity.
    + simpl; destruct IHn0.
      eexists; split.
      simpl in a; destruct a.
      apply e0.
      rewrite Bool.negb_involutive.      rewrite Bool.negb_involutive.
      simpl in a; destruct a.
      apply H.
Defined.

Extraction parity''.

Lemma parity' (n: nat) : {y | parity n = y} * {y | parity (pred n) = y}.
Proof.
  induction n as [|n0].
  - simpl. split; eexists; reflexivity.
  - destruct n0 as [|n1].
    + simpl. split; eexists; reflexivity.
    + split.
      * simpl. rewrite Bool.negb_involutive.
        destruct IHn0 as [IHSn1 IHn1]. apply IHn1.
      * destruct IHn0 as [IHSn1 IHn1]. apply IHSn1.
Defined.

 *)

Open Scope Z_scope.

Section Alt.

  Theorem sub_sub_distr : forall n m p : Z, n - (m - p) = (n - m) + p.
  Admitted.

  Theorem add_sub_assoc: forall n m p : Z, n + (m - p) = (n + m) - p.
  Admitted.


  Fixpoint alt (l: list Z) :=
    match l with
    | nil => 0
    | x::xs => x - alt xs
    end.

  Lemma alt' : forall (l: list Z), {y | alt l = y}.
    destruct l as [|x xs].
    - simpl; eexists; reflexivity.
    - simpl.
      generalize x xs;
        fix alt'1 2
        with (alt'2 (x: Z) (xs: list Z) {struct xs} : {y | x + alt xs = y}).
      + destruct xs0 as [|x' xs'].
        * simpl; eexists; rewrite Z.sub_0_r; reflexivity.
        * simpl. rewrite sub_sub_distr.
          generalize (x0 - x') xs'.   (* [{y : Z | x + alt xs = y}] *)
          apply alt'2.
      + destruct xs0 as [|x' xs'].
        * simpl; eexists; rewrite Z.add_0_r; reflexivity.
        * simpl. rewrite add_sub_assoc.
          apply alt'1.
  Defined.      

End Alt.
  
(* Holy shit x 2 !! *)
Extraction alt'.

Section Alt'.
  
  Fixpoint alt'1 (x : Z) (xs : list Z) {struct xs} : Z :=
    match xs with
    | nil => 0
    | cons x' xs' => alt'2 (x - x') xs'
    end
  with alt'2 (x : Z) (xs : list Z) {struct xs} : Z :=
         match xs with
         | nil => 0
         | cons x' xs' => alt'1 (x - x') xs'
         end.

End Alt'.