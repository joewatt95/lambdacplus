-- Formalization of Lawvere's fixed point theorem, which captures the essence of
-- the diagonal argument as found in Cantor's famous theorem on cardinality.

constant X : Type
constant Y : Type

-- Definition of equality.
-- Technically it's a type constructor parameterized by X : Type.
constant eq : forall (X : Type) (x : X) (y : X), Prop

-- Reflexivity of =
axiom eq_refl : forall (X : Type) (x : X), eq X x x

-- Symmetry of =
axiom eq_symm : forall (X : Type) (x : X) (y : X), (eq X x y) -> (eq X y x)

-- Transitivity of =
axiom eq_trans : 
    forall (X : Type) (x : X) (y : X) (z : X),
        (eq X x y) -> (eq X y z) -> (eq X x z)

-- If 2 functions are equal f = g, then f x = g x for every x : X.
axiom congr_fun : 
  forall (f : X -> Y) (g : X -> Y),
    (eq (X -> Y) f g) -> (forall x : X, eq Y (f x) (g x))

-- f has a fixed point if f x = x for some x : X.
def has_fixed_point :=
  fun (X : Type) (f : X -> X) => exists x : X, eq X (f x) x

-- f is surjective
def surjective :=
  fun (X : Type) (Y : Type) (f : X -> Y) => forall y : Y, exists x : X, eq Y (f x) y

theorem cantor : 
(exists g : X -> X -> Y, surjective X (X -> Y) g) -> forall f : Y -> Y, has_fixed_point Y f := 
  assume h (f : Y -> Y),
    -- Existential elimination to pull apart h.
    let g := fst h in
    have surjective X (X -> Y) g, from snd h,

    -- Define the diagonal function. This picks out the elements along the diagonal
    -- and flips them around using f.
    let diag : X -> Y := fun x => f (g x x) in

    -- Since g : X -> X -> Y, there must be some x : X with g x = diag
    -- Grab the witness, x, and the proof that g x = diag.
    have exists x : X, eq (X -> Y) (g x) diag, from this diag,
    let x := fst this in
    have eq (X -> Y) (g x) diag, from snd this,

    -- Next we have some boring manipulations using the axioms of equality to
    -- establish that (g x x) = f (g x x) and then flip the equality around.

    -- Since (g x) and diag are equal as functions, g x x = diag x
    have h1 : eq Y (g x x) (diag x), from
      congr_fun (g x) diag this x,

    -- diag x = f (g x x) must hold for this specific x we're working with because
    -- that's the definition of the diagonal function, diag
    have h2 : eq Y (diag x) (f (g x x)), from eq_refl Y (diag x),
    -- By transitivity, g x x = f (g x x)
    have eq Y (g x x) (f (g x x)), from 
      eq_trans Y (g x x) (diag x) (f (g x x)) h1 h2,
    -- By symmetry, f (g x x) = g x x
    have eq Y (f (g x x)) (g x x), from eq_symm Y (g x x) (f (g x x)) this,
 
    -- Use (g x x) and h to witness the existentially quantified statement that f
    -- has a fixed point.
    show has_fixed_point Y f, from (g x x, this)

check cantor