# MiniK

`MiniK` is an implementation of a rewriting engine heavily inspired by the [K Framework](https://github.com/runtimeverification/k).

It is a general rewriting engine, capable of executing any language modelled in the internal language of the rewriting engine.

It strives to have enough features to be able to run concrete programs from a simple imperative language, [Imp](https://github.com/ana-pantilie/MiniK/blob/main/src/Languages/Imp.hs).

## Build instructions
- `stack install` will build the executable, run with `MiniK-exe`: currently, this will run the Imp program defined in `app/Main.hs`
- `stack test` will run the tests from `test/Spec.hs`

## Haskell backend developer technical assessment
The following is the technical assessment for the position of Haskell backend developer at Runtime Verification.
We would like to see you understand the MiniK project, and make substantial improvements to it.

It needs to be able to run large enough programs, so that you can do a performance analysis on it and fix any performance-related issues. We want to see your process of identifying and fixing these kinds of issues, so please prepare a detailed report illustrating that as part of your submission.

The current code is far from perfect, we want to see your ability of indepedently understanding a new codebase, understanding our domain, finding and debugging issues, and improving code based on known best practices.

We want to see your best effort in taking this project and making it "production ready", with a large focus on performance, correctness and maintainability.

This assessment is desgined to mimic as much as possible the responsibilities you will have on the job. We want to be as transparent as possible with what we're looking for, and with what the position entails.


## TODOs:

Obviously, it would be easier, faster and most effecient -- to refactor the data structures first to encode business logic in elegant way with most suitable type guaranties, which guide appropriate functions refactoring. And do necessary optimizations in bottleneck places after that. But I choose other way: find and make step-by-step least necessary fixes to be able compare performance changes. Which take a lot more time, unfortunatelly, but corresponds to the task requirements. Also, I'll try to avoid make changes in the target language semantic (an Imp), and appropriate matching logic (or make a small necessary step-by-step fixes and performance improvements on demand only).

TODO: evaluate RHS of assign
TODO: evaluate nested sequences, e.g. by subsequential matching
???: how to introduce new vars for integers evaluation
TODO: variable AST that is sum of concrete AST and variable id
???: eval ints while match to vars (or while substitute back?)
TODO: test name clash for both ADT vars (even the case when few rules applyse in one turn, so inter-rules clash could appear) and target language vars
???: why renormalize necessary
???: why substitute AST at the left part of rule
TODO: split read and write interfaces for RewriteRule for left/right
TODO: avoid MapType when it's no need for extra variants (maybe, split to 2 separate types -- for konfiguration state and for MiniK AST (the program, and the right part of the rule, the -- are non-variable ones, and the left side of rule, the matched one -- variable rul).
TODO: not one rule to substitute per time, but back-tracking via Logic
!!!: keep guaranties that rules are not applied in overlap (prove by types their minimality). Also it would be nice to prove correctness.
FIXME: target language AST types are not sound. Make building incorrect AST statically impossible
???: should be such semantic properties as commutativity and associativity be added to match not syntactically exact, by semantically equal rules (of + and * for int, or for bool operations)? Or keep that property to be defined as a rule of target language itsel explicitly?
???: Of course, it would be better to not limit state values and identifiers by Int type only. Anyway, andy simple type inference gives target langue more expressive power then just types hardcoded in the AST sorts.

problems of the target language:
- lexical bindings
- non-modular rules for target language AST elements (their composition depend on seq composition)

The Imp language is almost Monoid, but has only left associativity, because of State (it would be associative, if RewriteRule's Konfiguration pair's kStates be considered as a function definition).

Is ChoiceT faster than LogicT?

KSeq is also looks very like Monoid:

(a ~> b) ~> c is the same as a ~> (b ~> c) -- KSeq must be associative.
. ~> a is the same as a ~> . and same as a -- KSeq must be left an right identity
concatination: mconcat = foldr (<>) mempty -- ??? How <> supposed to work here from the RewriteRule perspective? The result of the ~> <$> rule1 <*> rule2 would be Maybe Substitution (is it?)

So, MiniK is Monoid actually (and KSeq itself looks like an Alternative).

(a ~> b) ~> (c ~> d) -- should it be normalized ( e.g. to a ~> (b ~> (c ~> d)) ) ??? Or process as a tree? Or use CPS? Or yoneda-encoded fold? By the way, normalization could encoded explicitly by GADT.

Maybe, it worth to split KSeq from the MiniK explicitly -- to match MiniK with rules properly.
As a first approximation, I can just rewrite MiniK to [MiniK] (to use already normalized structure of the []) by dropping KSeq and KEmpty (the functor from CT perspective).

But what about KVar then, how they are supposed to be matched?

???: Rewrite rule: what if something in the new state depend on a previous part of the same state?
???: Use (Map Name IntType, Maybe Substitution) or even: StateT (Map Name IntType) Maybe Substitution -- for the matchWithConcrete result type
FIXME: Add test for map commutativity.
FIXME: At matchWithSymbolic wrap Logic.observeT with error handler in case of zero results. Add test for that.
FIXME: Add test to match empty map with non-opaque map. Fix if necessary (matchWithOpaque the Nothing case).
FIXME: Add test that KSeq inside KSymbol inherit substitutions properly. Fix it if necessary (return $ Substitution.multiUnion [subst1, subst2, subst]). The same for Plus, And, LT'
FIXME: Simplify IdVar pattern matching, avoid nested pattern hardcoding (e.g. by using back-tracking).
???: Should normalizeK wrap MiniK in KSeq if it not wrapped yet (to be compatibele with current rewrite rules)?

Substitution cache argument of matchWithSubstitution function make sense only for map types (as a state implementation at the moment, MapVar for current Imp only).


So, plan is to:

- [X] Read the codebase
- [X] Understand the code flow
- [X] Fix tests
- [ ] Write more tests (Add TestImp language for that)
- [ ] Rewrite datatypes as GADT to impove code maintainability
- [ ] Rewrite functions type signatures in a final tagless way to improve code modularity
- [ ] Estimate data processing algorithmic complexity, improve it where possible
- [ ] Write load test, profile code to find the bottlenecks, fix them if possible.

Profiling results of load test (average):

``` shell
test --trace --profile --ta '-p "Load tests"'

Test suite
  Imp tests
    Load tests
      Long nested while loop: OK (0.36s)
```

After the rewrite refactoring ~32s. (timestamps of events from eventloop is too precise for such a spread).

