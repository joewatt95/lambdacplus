theorem and_or_distrib_1 :
forall (P : Prop) (Q : Prop) (R : Prop),
(P * (Q + R)) -> (P * Q) + (P * R) :=
  assume P Q R (h : P * (Q + R)),
    have p : P, from fst h,
    have Q + R, from snd h,
    match this with
    | inl q =>
      have P * Q, from (p, q),
      show (P * Q) + (P * R), from inl this
    | inr r =>
      have (P * R), from (p, r),
      show (P * Q) + (P * R), from inr this
    end

theorem and_or_distrib_2 :
forall (P : Prop) (Q : Prop) (R : Prop),
(P * (Q + R)) -> (P * Q) + (P * R) :=
  assume P Q R (h : P * (Q + R)),
  let (p, q_or_r) := h in 
    match q_or_r with
    | inl q =>
      have P * Q, from (p, q),
      show (P * Q) + (P * R), from inl this
    | inr r =>
      have (P * R), from (p, r),
      show (P * Q) + (P * R), from inr this
    end

theorem _ : forall (A : Prop) (B : Prop) (C : Prop), (A /\ B) \/ (A /\ C) -> A /\ (B \/ C) :=
assume A B C h,
    let f := fun (X : Type) (a_and_x : A /\ X) (intro : X -> B \/ C) =>
        have X, from snd a_and_x,
        have B \/ C, from intro this,
        show A /\ (B \/ C), from (fst a_and_x, this)
    in
    match h with
    | inl a_and_b => f B a_and_b (fun b => inl b)
    | inr a_and_c => f C a_and_c (fun c => inr c)
    end