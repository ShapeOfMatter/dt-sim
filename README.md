---
title: PCL
subtitle: Probabilistic Choreography Language
author: Mako Bates
---

## Instructions

The first thing to try once you've got the code in place is

```bash
$>  cabal build --enable-tests
```

While cabal will generally build everything as-needed, this is an easy early way to see if you've got dependency problems.
Next you can run a bigger test:

```bash
$>  cabal test
```

This will run the ~~two~~ test suite~~s~~.
One test in the suite of "core" semantics is currently failing, and probably will be until we revisit encryption.

Two executables will be built: `haskell-semantics` and `d-tree-data`.

`haskell-semantics` will run a .cho file using the specified semantics.
Optional trailing `1` or `0` arguments will be used as secret inputs, which are otherwise all set to `1`.

```bash
$>  cabal run haskell-semantics examples/3partyXOR.cho
Inputs:  [(Variable {variable = "c_in"},True),(Variable {variable = "h1_in"},True),(Variable {variable = "h2_in"},True)]
Tapes:  [(Variable {variable = "c_s1"},True),(Variable {variable = "c_s2"},True),(Variable {variable = "h1_s1"},False),(Variable {variable = "h1_s2"},True),(Variable {variable = "h2_s1"},True),(Variable {variable = "h2_s2"},True)]
Views:  [(Party {party = "C"},[(Variable {variable = "c_s1"},True),(Variable {variable = "c_s2"},True),(Variable {variable = "h1_s3"},False),(Variable {variable = "h1_sum"},False),(Variable {variable = "h2_s3"},True),(Variable {variable = "h2_sum"},True)]),(Party {party = "H1"},[(Variable {variable = "c_s1"},True),(Variable {variable = "c_sum"},False),(Variable {variable = "h1_s1"},False),(Variable {variable = "h1_s2"},True),(Variable {variable = "h2_s1"},True),(Variable {variable = "h2_sum"},True)]),(Party {party = "H2"},[(Variable {variable = "c_s2"},True),(Variable {variable = "c_sum"},False),(Variable {variable = "h1_s2"},True),(Variable {variable = "h1_sum"},False),(Variable {variable = "h2_s1"},True),(Variable {variable = "h2_s2"},True)])]
Outputs:  [(Party {party = "C"},[(Variable {variable = "y"},True)]),(Party {party = "H1"},[(Variable {variable = "y"},True)]),(Party {party = "H2"},[(Variable {variable = "y"},True)])]

##  Set the first two arguments (c_in and h1_in as observed in the list above) to False:
$>  cabal run helloworld examples/3partyXOR.cho 0 0
Inputs:  [(Variable {variable = "c_in"},False),(Variable {variable = "h1_in"},False),(Variable {variable = "h2_in"},True)]
Tapes:  [(Variable {variable = "c_s1"},False),(Variable {variable = "c_s2"},True),(Variable {variable = "h1_s1"},True),(Variable {variable = "h1_s2"},True),(Variable {variable = "h2_s1"},False),(Variable {variable = "h2_s2"},False)]
Views:  [(Party {party = "C"},[(Variable {variable = "c_s1"},False),(Variable {variable = "c_s2"},True),(Variable {variable = "h1_s3"},False),(Variable {variable = "h1_sum"},True),(Variable {variable = "h2_s3"},True),(Variable {variable = "h2_sum"},False)]),(Party {party = "H1"},[(Variable {variable = "c_s1"},False),(Variable {variable = "c_sum"},False),(Variable {variable = "h1_s1"},True),(Variable {variable = "h1_s2"},True),(Variable {variable = "h2_s1"},False),(Variable {variable = "h2_sum"},False)]),(Party {party = "H2"},[(Variable {variable = "c_s2"},True),(Variable {variable = "c_sum"},False),(Variable {variable = "h1_s2"},True),(Variable {variable = "h1_sum"},True),(Variable {variable = "h2_s1"},False),(Variable {variable = "h2_s2"},False)])]
Outputs:  [(Party {party = "C"},[(Variable {variable = "y"},True)]),(Party {party = "H1"},[(Variable {variable = "y"},True)]),(Party {party = "H2"},[(Variable {variable = "y"},True)])]
```

`d-tree-data` uses the same semantics, but runs the program many times with random inputs.
You may choose to run it with `cabal exec` instead of `cabal run`
so you can pipe it someplace without the extra stuff `run` prints,
but _I think_ `exec` skips the "up to date" check, so make sure you run `cabal build` first!

```bash
## easy, prints to stdout:
$>  cabal run d-tree-data examples/adder_8.cho 2 2 2
## takes much longer, saves a ~2MB file:
$>  cabal exec d-tree-data examples/adder_8.cho 100 100 100 > examples/adder_8.csv
## crashes on my computer:
$>  cabal exec d-tree-data examples/adder_8.cho 1000 1000 1000 > examples/adder_8.csv
```

## Notes

- The python snippets used in code-generation are read at compile-time using quasi-quotes and template haskell!
- Inputs and outputs, tapes and views, are all handled as lookup tables by variable name.
  This helps make sense of what you're seeing in theory, but it causes problems once you get into functions.
  In the case of Views, we yield a list of values for each variable, in case it gets used more than once
  (but there's no assistance figuring out which is which).
  In the case of Output, _only the last value for each variable will be yielded_.

## Wishlist

- Operations:
  - [x] send
  - [x] input
  - [x] flip
  - [x] output
  - [x] xor, and
  - [x] OT
  - [ ] encrypt:
    - [ ] vector keys and ciphers
    - [ ] bit plaintext
    - [ ] enc(b, k): generate c at random, add ((c,k), b) to global lookup.
    - [ ] dec(c,k): lookup (c,k) in table, return b or random
      (or error, compare against the dificultly of yao's point-n-permute).
    - [ ] [Read for ideas](https://joyofcryptography.com/pdf/chap7.pdf)
- [x] Gathers views during evaluation.
- [x] Gathers output for each party.
- [x] Typechecking yields dimensions of input (and output and views)
- [ ] Typechecking yields party list.
