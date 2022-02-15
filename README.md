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
