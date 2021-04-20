# λC+
## Introduction
λC+ is a proof assistant implementing a typed lambda calculus that brings the Curry-Howard correspondence to life. 
At its core is an extensions of the Calculus of Constructions.

Try it out [here](https://aellym0.github.io/lambdacplus/)!

Here's an example formalizing the reflexivity of implication in λC+.
```lean
theorem A_implies_A :
-- For any proposition A, A implies A itself.
forall (A : Type), A -> A :=
-- A proof of this proposition is a function which takes a proposition A and
-- a proof of `A` and then returns a proof of `A`.
  fun (A : Type) (a : A) => a
```
Notice that the proof of this theorem looks a lot like the polymorphic identity
function.

Syntactically, λC+ looks and feels a lot like the 
[Lean theorem prover](https://leanprover.github.io/), which in turn resembles
Coq and Ocaml.

For those who are more comfortable with traditional pen and paper proofs,
rather than proofs written as programs, the above theorem can be rewritten using
some lightweight syntactic sugar:

```lean
theorem A_implies_A :
forall (A : Prop), A -> A :=
-- Assume that `A` is a type (which we can think of as a proposition), and that
-- `a` is a proof of `A`.
  assume (A : Prop) (a : A),
    -- We may conclude `A` because `a` is a proof of it.
    show A, from a
```

where
* `Prop` is a synonym for `Type`. Writing `Prop` emphasizes the role of `A` as
a _proposition_.

* `assume ...` and `show ..., from ...` are syntactic sugar inspired by Lean's
[structured proofs terms](https://leanprover.github.io/reference/expressions.html#structured-proofs).

We hope that this helps improve the readability of proofs for those who are more
used to traditional natural deduction proofs.

## Documentation
To learn more about the project, please see
[our wiki](https://github.com/aellym0/lambdacplus/wiki/1.-Home).