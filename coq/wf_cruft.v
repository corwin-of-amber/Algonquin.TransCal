
Import Wf.

Require Import Lt.



Section WfLt.

  (* Library already has lt_wf, but here's a less generic proof *)
  Lemma lt_wf'' : forall y x, x < y -> Acc lt x.
    induction y.
    - intros x Hx; inversion Hx.
    - unfold lt. intros x Hx.
      econstructor.
      intros.
      apply IHy.
      eapply lt_le_trans; try apply H.
      auto with arith.
  Defined.

  Lemma lt_wf' : well_founded lt.
    red. intros.
    eapply lt_wf''.
    constructor.
  Defined.

End WfLt.

Section WfOf.

  Variable A B : Type.
  Variable lt : B -> B -> Prop.
  Variable f : A -> B.
  Local Definition ltof (a' a : A) := lt (f a') (f a).

  Hypothesis lt_wf : well_founded lt.

  Theorem wf_of : forall x, Acc ltof x.
    intro x.
    refine (Fix lt_wf (fun fx => forall x, fx = f x -> Acc ltof x) (fun fx wf_of' x => _) (f x) x eq_refl).
    - intro.
      constructor.
      unfold ltof at 1.
      intros.
      rewrite <- H in H0.
      apply (wf_of' _ H0).
      reflexivity.
  Defined.

End WfOf.

Require Import Wellfounded.Lexicographic_Product.
Require Import Relations.Relation_Operators.



Section WfLex.

  Section Pair.

    Variables A B : Type.
    
    Let pair_to_sigT {A B} := fun ab:A*B => let (a,b) := ab in existT (fun _ => B) a b.

    Definition lexprod_pair lta (ltb : B -> B -> Prop) (ab' ab : A*B) :=
      lexprod A (fun _ => B) lta (fun _ => ltb) (pair_to_sigT ab') (pair_to_sigT ab).

    Require Import Wellfounded.Inverse_Image.
    
    Theorem wf_lexprod_pair lta ltb :
      well_founded lta -> well_founded ltb -> well_founded (lexprod_pair lta ltb).
      intros.
      apply wf_inverse_image with (f := pair_to_sigT).
      apply wf_lexprod; intros; assumption.
    Qed.

  End Pair.

  Section Triple.
    
    Variable A B C : Type.

    Definition lexprod_triple lta (ltb : B -> B -> Prop) (ltc : C -> C -> Prop) :=
      lexprod_pair _ _ (lexprod_pair A B lta ltb) ltc.

    Theorem wf_lexprod_triple lta ltb ltc :
      well_founded lta -> well_founded ltb -> well_founded ltc -> well_founded (lexprod_triple lta ltb ltc).
      intros.
      apply wf_lexprod_pair.
      - apply wf_lexprod_pair; assumption.
      - assumption.
    Defined.
    
  End Triple.
  
End WfLex.



Section WfPrune.

  Variable A : Type.
  Variables R R' : A -> A -> Prop.

  Hypothesis H : forall a b, R' a b -> R a b.
  
  Theorem wf_prune' : forall x, Acc R x -> Acc R' x.
    intros.
    induction H0.  
    econstructor.
    intros.
    apply H1, H, H2.
  Defined.

  Theorem wf_prune : well_founded R -> well_founded R'.
    red; intros.
    apply wf_prune', H0.
  Defined.

End WfPrune.


(*
 * This section is a little bit contrived
 *)
Section WfLax.

  Variable A : Type.
  Variable P : A -> Prop.
  Variable R : A -> A -> Prop.

  Hypothesis P_dec : forall x, {P x} + {~P x}.
  
  Definition PR a' a := ~P a /\ (P a' \/ R a' a).

  Lemma P_Acc z : P z -> Acc PR z.
    intros; econstructor.
    unfold PR; tauto.       (* P z contradict ~P z /\ ... in premise *)

  Defined.
  
  Theorem wf_lax' : forall x, Acc R x -> Acc PR x.
    intros x H.
    induction H.
    constructor. unfold PR at 1; intros.
    case (P_dec y).
    - apply P_Acc.
    - intros; apply H0; tauto.
  Defined.

  Theorem wf_lax : well_founded R -> well_founded PR.
    red; intros.
    apply wf_lax', H.
  Defined.

  Variable R' : A -> A -> Prop.
  Hypothesis R'_def : forall a b, R' a b <-> PR a b.
  
  Theorem wf_elax : well_founded R -> well_founded R'.
    intros.
    eapply wf_prune.
    apply R'_def.
    apply wf_lax.
    assumption.
  Defined.

End WfLax.
