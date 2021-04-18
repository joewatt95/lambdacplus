constant A : Type
constant P : A -> A -> Prop

theorem _ :
(forall (x : A) (y : A), P x y) -> (forall (y : A) (x : A), P x y) :=
  fun f => fun y x =>
    let h : P x y := f x y in
    h

theorem _ :
(forall (x : A) (y : A), P x y) -> (forall (y : A) (x : A), P x y) :=
  assume f y x,
    show P x y, from f x y

constant P : A -> Prop
constant Q : A -> Prop
constant R : Prop

theorem _ :
(exists x : A, R) -> R :=
  assume h,
  let {x, r} := h in r

def _ := 
  fun (h : exists x : A, P x) => 
    let {x, h} := h in (h : P x)

theorem _ :
(forall x : A, (P x) /\ (Q x)) -> (forall x : A, P x) /\ forall x : A, Q x :=
  assume (h : forall x : A, (P x) /\ (Q x)),
    have h1 : forall x : A, P x, from
      fun x => fst (h x),
    have h2 : forall x : A, Q x, from
      fun x => snd (h x),
    show (forall x : A, P x) /\ forall x : A, Q x, from (h1, h2)

theorem _ :
(exists x : A, (P x) \/ (Q x)) -> (exists x : A, P x) \/ exists x : A, Q x :=
  assume h,
    let {x, h} := h in
    -- h is a proof of P x or Q x
    match h with
    | inl px =>
      have exists x : A, P x, from {x, px},
      show (exists x : A, P x) \/ exists x : A, Q x, from inl this 
    | inr qx =>
      have exists x : A, Q x, from {x, qx},
      show (exists x : A, P x) \/ exists x : A, Q x, from inr this 
    end

def _ : (exists x : A, P x) -> P x :=
  fun (h : exists x : A, P x) => 
    let {x, h} := h in (h : P x)