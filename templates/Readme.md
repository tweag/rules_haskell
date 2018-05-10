This directory contains manually-checked-in data-files that Happy and Alex use
and which are generated automatically by their custom `Setup.hs` scripts.
Figuring out a better solution is TODO; however, those files' interfaces
have been *very* stable, and upcoming releases of those packages
will include them, so this seems OK for now.

Instructions for obtaining them:

- `alex`: They are now bundled with the latest release, `alex-3.2.4`, in the
  `data` folder.
- `happy`: Run the following commands to generate the data-files:

      cabal update
      cabal unpack happy
      cd happy-*
      cabal build

   Then the data files will be located in the root of the Cabal package
   directory.
