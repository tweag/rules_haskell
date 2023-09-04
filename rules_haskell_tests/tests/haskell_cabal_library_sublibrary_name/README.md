The `package1.tar` archive contains a copy of the cabal package from the current directory.

It is there so that this cabal package can be used in a custom stack snapshot this way:

```
  - archive: https://github.com/tweag/rules_haskell/raw/xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx/tests/haskell_cabal_library_sublibrary_name/package1.tar
    sha256: "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
```

It is used in particular to test support of cabal sublibraries in the `stack_snapshot` rule.
