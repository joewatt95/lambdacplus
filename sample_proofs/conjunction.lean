theorem and_comm :
forall (P : Prop) (Q : Prop), (P /\ Q) -> (Q /\ P) :=
  assume P Q (h : P /\ Q),
    have p : P, from fst h,
    have q : Q, from snd h,
    show Q /\ P, from (q, p)

theorem and_assoc :
forall (P : Prop) (Q : Prop) (R : Prop), ((P /\ Q) /\ R) -> (P /\ (Q /\ R)) :=
  assume P Q R (h : (P /\ Q) /\ R),
    have r : R, from snd h,
    have P /\ Q, from fst h,
    have p : P, from fst this,
    have q : Q, from snd this,
    have Q /\ R, from (q, r),
    show P /\ (Q /\ R), from (p, this)

check and_assoc