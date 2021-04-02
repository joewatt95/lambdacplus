-- The axiom of choice is provable constructively due to the strong elimination rule
-- of the Sigma type.
theorem choice : 
forall (A : Type) (B : Type) (R : A -> B -> Prop),
(forall a : A, Sigma b : B, R a b) -> Sigma f : A -> B, forall a : A, R a (f a) := 
  assume A B R R_left_total,
    -- Define the magic choice function :^)
    -- This looks into the proof that R is left total and grabs an explicit
    -- witness for a.
    let f : A -> B := fun a =>
      have Sigma b : B, R a b, from R_left_total a,
      fst this
    in
    have forall a : A, R a (f a), from
      assume a,
        have Sigma b : B, R a b, from R_left_total a,
        show R a (f a), from snd this,
    show Sigma f : A -> B, forall a : A, R a (f a), from (f, this)

check choice

theorem choice1 :
forall (A : Type) (B : Type) (R : A -> B -> Prop),
(forall a : A, exists b : B, R a b) -> forall a : A, Sigma b : B, R a b := 
  assume A B R R_left_total,
  fun (a : A) =>
    have exists b : B, R a b, from R_left_total a,
    exists_elim this
    assume (b : B) (h : R a b), ((b, h) : Sigma b : B, R a b)

theorem choice3 :
forall (A : Type) (B : Type) (R : A -> B -> Prop),
(Sigma f : A -> B, forall a : A, R a (f a)) -> (exists f : A -> B, forall a : A, R a (f a)) :=
  assume A B R h,
    let f := fst h in
    have forall a : A, R a (f a), from snd h,
    show exists f : A -> B, forall a : A, R a (f a), from (|f, this|)

axiom A : Type
axiom B : Type
axiom R : A -> B -> Prop
axiom R_left_total : forall a : A, exists b : B, R a b

def f := fun (a : A) =>
      have exists b : B, R a b, from R_left_total a,
      let (b, Rab) := this in b

eval f
