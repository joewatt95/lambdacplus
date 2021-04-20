-- Commutativity of conjunction
theorem or_comm :
forall (P : Prop) (Q : Prop), (P \/ Q) -> Q \/ P :=
  assume P Q (h : P \/ Q),
  show Q \/ P, from
    match h with
    | inl p => (inr p : Q \/ P)
    | inr q => (inl q : Q \/ P)
    end

check or_comm

-- Associativity of conjunction
theorem or_assoc :
forall (P : Prop) (Q : Prop) (R : Prop), ((P \/ Q) \/ R) -> (P \/ (Q \/ R)) :=
  assume P Q R (h : (P \/ Q) \/ R),
    match h with
    | inr r =>
      have Q \/ R, from inr r,
      show P \/ (Q \/ R), from inr this
    | inl p_or_q =>
      match p_or_q with
      | inl p =>
        show P \/ (Q \/ R), from inl p
      | inr q =>
        have Q \/ R, from inl q,
        show P \/ (Q \/ R), from inr this
      end
    end

check or_assoc