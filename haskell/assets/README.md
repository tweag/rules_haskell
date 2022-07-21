The most recent patches for Windows were generated using the following process:

1. Download the ghc version for windows you are interested in (following the link provided in `haskell/private/ghc_bindist_generated.bzl`).

2. Open it.

3. Create a commit with the lib/package.conf.d` folder.

``` sh
git init
git add lib/package.conf.d
git commit -m "Packages"
```

4. Modify the paths relative to `${pkgroot}`

``` sh
cd lib/package.conf.d/
sed -i "s/{pkgroot}\/\.\.\/\.\.\//{pkgroot}\/\.\.\//g" *.conf
cd ../..
```

5. Commit the new version.

``` sh
git commit -am "Patched"
```

6. Get the commit hash of the previous version.

``` sh
git log
```

7. Generate the patch.

``` sh
git diff --no-prefix HASH_OF_FIRST_COMMIT > ghc_VERSION_NUMBER_win.patch
```

8. Remove parasitic lines.

``` sh
sed -i "/^index \|^diff /d" ghc_VERSION_NUMBER_win.patch
```

9. Copy it to this folder.
