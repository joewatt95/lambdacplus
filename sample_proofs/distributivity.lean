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