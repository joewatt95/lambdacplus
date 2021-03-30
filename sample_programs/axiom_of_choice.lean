constant A : Type
constant B : Type

constant R : A -> B -> Prop

-- The axiom of choice is provable constructively due to the strong elimination rule
-- of the Sigma type.
theorem choice : 
(forall a : A, exists b : B, R a b) -> exists f : A -> B, forall a : A, R a (f a) := 
  assume R_left_total,
    -- Define the magic choice function :^)
    -- This looks into the proof that R is left total and grabs an explicit
    -- witness for a.
    let f := fun (a : A) =>
      have exists b : B, R a b, from R_left_total a,
      fst this
    in
    have forall a : A, R a (f a), from
      assume a,
        have exists b : B, R a b, from R_left_total a,
        show R a (f a), from snd this,
    show exists f : A -> B, forall a : A, R a (f a), from (f, this)

check choice