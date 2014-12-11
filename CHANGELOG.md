# Changelog

# 0.2.3

- updated .mli to replace "enum" with "gen"
- Gen.persistent_lazy now exposes caching parameters related to GenMList.of_gen_lazy
- give control over buffering in GenMList.of_gen_lazy
- move some code to new modules GenClone and GenMList
- add lwt and async style infix map operators
- Gen.IO
- to_string, of_string, to_buffer
- opam file
- add permutations_heap for array-based permutations; add a corresponding benchmark to compare
- license file

# 0.2.2

- do not depend on qtest
- better combinatorics (permutations, power_set, combinations)
- Gen.{permutations,power_set,combinations}
- Gen.unfold_scan
- put Gen.S into a new module, Gen_intf
- Gen.persistent_lazy implemented
- .merlin files

## 0.2.1

- added many tests using Qtest; fixed 2 bugs
- simpler and more efficient unrolled list
- unrolled list for Gen.persistent (much better on big generators)

## 0.2

- changed camlCase to this_case
- take_nth combinator

note: git log --no-merges previous_version..HEAD --pretty=%s

