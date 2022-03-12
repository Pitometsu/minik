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

As I understand, the current data flow is:
- iterate over list of rewrite rules unless one's left side match the tree of program's AST and State (and rule's condition is satisfied)
- save matched substitutions to the substitution Map (using State to dereference identifiers)
- substitute matches to the right side of that rewrite rule (AST and State)
- use right side of the rewrite rule as a result, repeat with it, until there's no rules that matches left
- return substituted right side of the latest matched rewrite rule as a result of evaluation.

Maps preserve associativity and commutativity by normalization and key sorting. For commutativity of MiniK AST (KSeq nodes) I added normalization too (before the whole evaluation and after each rewriting).

???: Can normalization speed be improved? By encoding normalization invariant in KSeq data design.

???: Can the current data flow itself be made more efficient by design (not only implementation)? Possible, if rewrite rules could apply simultaneously, but that would be a whole design change.

It can't be concurrent if rules order matter, but it can be run in parallel (also there could be used other data structure instead of [] to speedup results concatination back: difference list, or LogicT, or Stream from steamly library, or Par from monad-par).

``` haskell
rewriteStep' konfig rewriteRules =
    Logic.observeT $ (lift . applyRewriteRule konfig) `parBind` scatter rewriteRules
  where
    parBind f = withStrategy (parTraversable rdeepseq) . (=<<) f
```

But it require `Traversable (LogicT m)` which is complicated (here's an implementation, but don't want to add extra dependensies like lenses for that small challenge: https://github.com/lih/BHR/blob/677b5fbe36578819fe9cb77e566475df8afb0b62/definitive-base/src/Algebra/Monad/Logic.hs#L23).

Also `Traversable (LogicT m)` would help to parallel the `generateMatch`.

Maybe in general in worth to implement KSeq similar to LogicT, with a parallel Functor instance -- for better performance.  But at the moment it's enough to use [] intermediate representation.

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
???: is MapType via MapCons implementation necessary as non-efficient? Would opaque interface like usual Map works?
???: is NormalizedMap matching efficient, can it be speed up?

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
- [X] Estimate data processing algorithmic complexity, improve it where possible
- [X] Write load test, profile code to find the bottlenecks, fix them if possible.

Profiling results of load test:

1st run

``` shell
stack test --trace --profile --ta '-p "Load tests"'

Test suite
  Imp tests
    Load tests
      Long nested while loop: OK (0.36s)
```

From eventloop: 0.026269s - 0.395897s = 0.369628s

2nd run

From eventloop: 0.006678s - 0.376399s = 0.369721s

3d run

From eventloop: 0.004312s - 0.373213s = 0.368901s

Average: 0.369417s


After the rewrite refactoring ~32s. (timestamps of events from eventloop is too precise for such a spread).

After making data fields strict ~16s (with strict Map ~17s).

With parallel rewrite step ~12s.
With parallel matches ~14s (get worse)
After rollback parallel for matches, except KSymbol and matchWithConcrete ~11s.


Made a decision to avoid GADTs: they are not strictly necessary for good enough type guaranties of that language.  However the KSymbol type could be indexed with product of appropriate symbol and type list of KType elements (to avoid error with wrong symbol AST).

Looks like it still required to have an KSeq on the right side of the RewriteRule to concatinate the matching results. To avoid it and keep the results normalized without extra steps, it would require matching alhorithm changing (making more complicated), which is kinda out of scope of the task.


What if int term like a + b will be matched with a + (IntVar x)? The Substitution contains MiniK values only, so the result of substitution would be a + (KInt b)? Which make no sense. Currently it uses `retractIntTerm :: MiniK -> IntType` unsafe function for that. Most proper approach would be to use GADT here. But I would just few Maps per each time (for deriving instances simplicity).

Looks like we can skip initial `coversAllRuleVars` check, because positive case is not rely on it, and rely on later Maybe result (to not spend program's time on checking it twice, and do it in typesafe way for better maintainability).

???: can we speedup `extractVariables` (e.g. by passing result cache as an additional parameter)? And `substituteVariable` (use some kind of function that build the tree instead of passing values?). Make them parallel.

TODO: try to speedup data by using unboxed values.
