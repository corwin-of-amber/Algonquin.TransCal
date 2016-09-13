
Require Import PeanoNat.

Require Import Coq.Init.Wf.
Require Import Coq.Arith.Wf_nat.
Require Import Wellfounded.Inverse_Image.

Require Import Lt.

Require Import wf_cruft.


Ltac dismiss := eexists; reflexivity.  (* used to finish a goal {y | y = ...} in the obvious way *)


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

  Lemma absdiff_0_n n : absdiff 0 n = n.
    unfold absdiff; lia.
  Qed.
  
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

    (* Extract the whole thing as a lemma so that it does not get expanded
     * when unfolding gcd *)
    Lemma gcd_d a b ab (Hab : ab = (a, b)) n (Hb : b = S n) : lt123' (absdiff a b, min a b) ab.
      subst.
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
      
    apply (gcd_d a b ab Hab n Hb).
  Defined.

  Lemma gcd_unroll_once a b :
    gcd a b = match b with
              | 0 => a
              | S _ => gcd (absdiff a b) (min a b)
              end.
    unfold gcd.
    rewrite Fix_eq at 1.
    - destruct b; reflexivity.
    - (* extensionality proof *)
      admit.
  Admitted.
  
  Lemma gcd_k a b k : k >= 1 -> a >= k * b -> gcd a b = gcd (a - k * b) b.
    destruct b.
    - (* b = 0  is a degenerate case *)
      intros.
      rewrite gcd_unroll_once, gcd_unroll_once.
      lia.
    - intro H; induction H.
      + (* k = 1 *)
        intro.
        rewrite gcd_unroll_once.
        unfold absdiff; f_equal; lia.
      + (* k > 1 *)
        intro; rewrite IHle; try lia.
        rewrite gcd_unroll_once.
        unfold absdiff; f_equal; lia.
  Qed.

  (*
     Nat.div_mod:
       forall x y : nat, y <> 0 -> x = y * (x / y) + x mod y
   *)

  Lemma div_mul_le a b : b <> 0 -> (a / b) * b <= a.
    intro.
    rewrite Nat.mul_comm.
    erewrite (Nat.div_mod a b) at 2; try assumption.
    apply Plus.le_plus_l.
  Qed.

  Check right_lex.

  Lemma right_lex_pair A B ltA (ltB : _ -> _ -> Prop) x y y' :
    ltB y y' -> lexprod_pair A B ltA ltB (x, y) (x, y').
    intros; apply right_lex; assumption.
  Qed.
    
  Lemma right_lex_pair' A B ltA (ltB : _ -> _ -> Prop) x x' y y' :
    x = x' -> ltB y y' -> lexprod_pair A B ltA ltB (x, y) (x', y').
    intro; subst. apply right_lex_pair.
  Qed.

  Lemma lt123'_hole a b : lt123' a b.  (** obviously false **)
  Admitted.

  Lemma gcd_comm a b : gcd a b = gcd b a.
    induction (lt123'_wf (a,b)).
    rewrite gcd_unroll_once.
    rewrite (gcd_unroll_once b).
    case b; case a; try reflexivity.
    - intro; rewrite min_l; try apply le_0_n.
      rewrite gcd_unroll_once; compute; reflexivity.
    - intro; rewrite min_l; try apply le_0_n.
      rewrite gcd_unroll_once; compute; reflexivity.
    - intros; f_equal.
      unfold absdiff;lia. lia.
  Qed.
  
  Lemma gcd' a b : {y | y = gcd a b}.
    pose (lt123'_wf (a,b)) as acc.
    generalize a b acc; fix h 3.
    intros.
    destruct (Nat.eq_dec b0 0).
    - subst; rewrite gcd_unroll_once; dismiss.
    - destruct (le_lt_dec b0 a0).
      + erewrite gcd_k.      
        Focus 3.
        * apply div_mul_le; assumption.
        * edestruct h as [r R]. Focus 2. rewrite <- R. dismiss.
          destruct acc0.
          apply H. (**) apply lt123'_hole. (**)
        * (* a >= b |- a / b >= 1 *)
          apply Nat.div_le_lower_bound; try assumption.
          rewrite Nat.mul_1_r; assumption.
      + rewrite gcd_unroll_once.
        generalize n; destruct b0 at 1 2; try tauto.
        intro. unfold absdiff. rewrite max_r, min_l.

  Defined.
        
End Euclid.


Extraction gcd.

Extraction gcd'.