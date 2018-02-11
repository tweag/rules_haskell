HaskellPackageInfo = provider(
  doc = "Package information exposed by Haskell libraries.",
  fields = {
    "name": "Package name, usually of the form name-version.",
    "names": "All package names of transitive dependencies. Includes own name.",
    "confs": "Package conf files.",
    "caches": "Package cache files.",
    "static_libraries": "Compiled library archives.",
    "dynamic_libraries": "Dynamic libraries.",
    "interface_files": "Interface files belonging to the packages.",
    "prebuilt_dependencies": "Transitive collection of all wired-in Haskell dependencies.",
    "external_libraries": "Dynamic shared libraries needed for linking. List of .so files.",
    "haddock_ghc_args": "Arguments that were used to compile the package, suitable for Haddock."
  }
)

HaddockInfo = provider(
  doc = "Haddock information.",
  fields = {
    "outputs": "All interesting outputs produced by Haddock.",
    "interface_file": "Haddock interface file.",
    "doc_dir": "Directory where all the documentation files live.",
  }
)
