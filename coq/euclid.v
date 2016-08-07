
Require Import PeanoNat.

Require Import Coq.Init.Wf.
Require Import Coq.Arith.Wf_nat.
Require Import Wellfounded.Inverse_Image.

Require Import Lt.

Require Import wf_cruft.



Section Euclid.

  Definition absdiff a b := max a b - min a b.

  Definition ltnz (ab' ab : nat * nat) :=
    let (a', b') := ab' in
    let (a, b) := ab in
    b <> 0 /\ (b' = 0 \/ max a' b' < max a b).

  Require Import Wellfounded.Lexicographic_Product.
  Require Import Relations.Relation_Operators.
  
  Check wf_symprod.
  Check symprod.

  Definition measure1 (ab : nat*nat) :=
    match ab with
    | (_, 0) => 0
    | (0, _) => 1
    | _ => 2
    end.
  Definition measure2 (ab : nat*nat) := let (a,b) := ab in max a b.

  Definition measure12 ab := (measure1 ab, measure2 ab).

  Print sigT.
  Definition lt12 ab' ab := lexprod_pair _ _ lt lt (measure12 ab') (measure12 ab).


  Lemma lt12_wf : well_founded lt12.
    apply wf_inverse_image with (f := measure12).
    - apply wf_lexprod_pair; apply lt_wf.
  Defined.

  Notation "A <>? B" := (negb (Nat.eqb A B)) (at level 70).
  
  Definition measure1' (ab : nat*nat) := let (a,b) := ab in b <>? 0.
  Definition measure2' (ab : nat*nat) := let (a,b) := ab in a <>? 0.
  Definition measure3' (ab : nat*nat) := let (a,b) := ab in max a b.

  Definition measure123' (ab : nat*nat) := (measure1' ab, measure2' ab, measure3' ab).

  Definition bool_lt a b := a = false /\ b = true.

  Let f := (fun (b:bool) => if b then 1 else 0).  (* used for following proof *)

  Lemma bool_lt_wf : well_founded bool_lt.
    Check ltof.
    apply wf_prune with (R := ltof bool f).
    - destruct a,b; unfold bool_lt; intro H; destruct H; try (discriminate H || discriminate H0). constructor.
    - apply wf_inverse_image with (f := f).
      Check lt_wf.
      apply lt_wf.
  Defined.
    
  Definition lt123' ab' ab := lexprod_triple _ _ _ bool_lt bool_lt lt (measure123' ab') (measure123' ab).

  Lemma lt123'_wf : well_founded lt123'.
    apply wf_inverse_image with (f := measure123').
    apply wf_lexprod_triple; apply bool_lt_wf || apply lt_wf.
  Defined.

      
  Lemma ltnz_wf : well_founded ltnz.
    eapply wf_elax with (P := fun x => snd x = 0) (R := fun x y => max (fst x) (snd x) < max (fst y) (snd y)).
    - intro; apply Nat.eq_dec.
    - case a,b; unfold PR; simpl. tauto.
    - apply well_founded_ltof.
  Defined.

  Require Import Compare_dec.

  Lemma neq_minmax_gt {a b} : a <> b -> max a b > min a b.
    intro neq.
    destruct (lt_eq_lt_dec a b); try destruct s; try tauto.  (* a = b is impossible *)
    - (* a < b *)
      rewrite max_r, min_l; try ( apply Le.le_Sn_le; assumption ).
      assumption.
    - (* b < a *)
      rewrite max_l, min_r; try ( apply Le.le_Sn_le; assumption ).
      assumption.
  Qed.

  Lemma neq_absdiff_nz {a b} : a <> b -> absdiff a b <> 0.
    intro neq.
    apply Nat.sub_gt.
    apply neq_minmax_gt.
    assumption.
  Qed.

  Lemma S_not_eq n m : S n <> S m -> n <> m.
    intros K H.
    absurd (S n = S m).
    - assumption.
    - rewrite H; reflexivity.
  Qed.

  Lemma eq_absdiff_z {a} : absdiff a a = 0.
    unfold absdiff.
    rewrite min_l, max_l; try constructor.
    apply Nat.sub_diag.
  Qed.

  Require Import Psatz.
  
  Definition gcd : forall (a b : nat), nat.
    intros.
    refine (Fix lt123'_wf (fun _ => nat)

                (fun (ab:nat*nat) gcd' =>
                   (let '(a,b) as k := ab return (ab = k -> _) in
                    fun Hab =>
                    match b as k return (b = k -> _) with
                    | O => fun _ => a
                    | S _ => fun Hb => gcd' (absdiff a b, min a b) _ 
                    end eq_refl) eq_refl

           ) (a, b)).

    - subst; clear f k.
      destruct a as [|a'].
      + (* a = 0 /\ b <> 0 *)
        apply left_lex, left_lex. simpl.
        red; split; reflexivity.     (* first component of measure123' is decreasing *)
      + (* a <> 0 /\ b <> 0 *)
        red.
        case (Nat.eq_dec a' n).
        * intro; subst. simpl.   (* a = b *)
          apply left_lex, right_lex. simpl. rewrite eq_absdiff_z. simpl.
          red; split; reflexivity.   (* second component is decreasing *)
        * intro a_neq_b.                             (* a <> b *)
          subst.
          unfold measure123'.
          simpl. replace (_ <>? 0) with true.
          (**) apply right_lex.      (* third component is decreasing *)
               unfold absdiff; lia.  (* <- very impressive *)
          (**) symmetry.
               rewrite Bool.negb_true_iff. (* negb b = true  -->  b = false *)
               rewrite Nat.eqb_neq.        (* (n =? m) = false  -->  n <> m *)
               auto using neq_absdiff_nz.
  Defined.

  
  
  
End Euclid.


Extraction gcd.