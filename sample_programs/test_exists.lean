-- This is supposed to typecheck fine.
theorem h1 :
forall (T : Type) (P : T -> Prop), (exists x : T, P x) -> (exists x : T, P x) :=
  assume T P (h : exists x : T, P x),
  exists_elim h assume (x : T) (px : P x),
    show exists x : T, P x, from (|x, px|)

-- This should also be ok.
axiom T : Type
axiom P : T -> Prop
axiom h : exists x : T, P x

def h1 := 
  exists_elim h 
  assume (x : T) (h : P x), ((|x, h|) : exists x : T, P x)

check h1

-- This is not legal.
def h1 := fun (T : Type) (P : T -> Prop) (h : exists x : T, P x) => 
  exists_elim h
  assume (x : T) (h : P x),
    fun (z : T) => h