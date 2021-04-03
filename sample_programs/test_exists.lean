-- This should also be ok.
axiom T : Type
axiom P : T -> Prop
axiom h : exists x : T, P x

def h1 := 
  let z := T in
  let {x, h} := h in
  ({x, h} : exists x : T, P x)

check h1

-- This is not legal.
def h1 := fun (T : Type) (P : T -> Prop) (h : exists x : T, P x) => 
  let {x, h} := h in
  fun (z : T) => h

-- This is valid.
theorem h1 :
forall (T : Type) (P : T -> Prop), (exists x : T, P x) -> (exists x : T, P x) :=
  assume T P (h : exists x : T, P x),
  -- Note that when eliminating the existential quantifier, the witness x is
  -- completely arbitrary save for the fact that it satisfies P x.
  have exists x : T, P x, from h,
  let {x, px} := this in
    show exists x : T, P x, from {x, px}


-- This is also valid.
theorem h1 :
forall (T : Type) (P : T -> Prop) (Q : T -> Prop),
(exists x : T, (P x) /\ (Q x)) -> (exists x : T, P x) /\ (exists x : T, Q x) :=
  assume T P Q (h : exists x : T, (P x) /\ (Q x)),
  -- Existential elimination.
  -- Note that the rule of existential elimination says that x should not occur 
  -- free the resulting type. In other words, we must discharge it before we
  -- can conclude the proof.
  let {x, h} := h in
  have px : P x, from fst h,
  have qx : Q x, from snd h,
  -- Discharge the witness x from our hypotheses.
  have exists_px : exists x : T, P x, from {x, px},
  have exists_qx : exists x : T, Q x, from {x, qx},
  show (exists x : T, P x) /\ (exists x : T, Q x), from (exists_px, exists_qx)