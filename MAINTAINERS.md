# Maintenance instructions

## Cutting a new release

- Copy this list of steps into a `rules_haskell` issue
- Create & checkout release branch (`release-<major>.<minor>`).
- [ ] Fix minimal bazel version in [the `start` script](./start),
  [haskell/private/versions.bzl](./haskell/private/versions.bzl), and
  [the README](./README.md); add to the [`CHANGELOG`](./CHANGELOG.md)
  if it changed.
- [ ] Remove any feature that is still too experimental to go into a
  release, by cherry-picking reverts (or by manually deleting the
  feature).
  - Check the list below for PRs to remove.
- [ ] Write `CHANGELOG` by going through all pull requests since last
  release (https://github.com/tweag/rules_haskell/is%3Apr+merged%3A>%3DYYYY-MM-DD)
  replacing YYYY-MM-DD by the date of the last release.
  - Add Highlights section for major improvements/changes
  - `Added`, `Removed`, `Changed`, `Fixed` as necessary
  - If relevant, add corresponding PRs to the changes
- [ ] Open a PR from the release branch, go through review and merge
  - If there is changes on the release branch that should not go to
    master, you create a second branch `release-<major>.<minor>-master`
    on master and cherry-pick all relevant commits from the release
    branch. Open a pull request with that branch, go through review
    and push changes back to the release branch.
- [ ] Verify that the final version of the release branch is green
- [ ] Create release tag (`v<major>.<minor>`) on the release branch and
  push it: `git push origin v<major>.<minor>`
- [ ] Go to the [release page](https://github.com/tweag/rules_haskell/releases)
    - click on “Draft a new release”
    - name `v<major>.<minor>`
    - select the tag you created in the previous item, it should be in
      github's dropdown list
    - copy the changelog section into the description
    - you don't need to provide the source `.zip` and `.tar.gz` archives,
      github does it automatically.
    - release
- [ ] Push `rules_haskell` version in start script to new release tag,
      test it in a temporary directory, create PR against master
  - [ ] Once PR has been merged, publish start script and website
        by checkouting the `release` branch and merging master into it.
        Then push the `release` branch, to trigger deployment.
  - [ ] Check whether https://haskell.build/start has the newest start
        script (netlify has problems sometimes).
- [ ] Announce the new version (on Twitter) by asking someone with Twitter
      rights.

### Remove these PRs from minor releases

- --


## Bumping bazel version

`rules_haskell` should always support the latest LTS release of bazel.

We strive to always test against the latest non-LTS release
nonetheless, so bumping bazel regularly is required.

- [ ] Use `.ci/update-bazel-version` to update `.ci/bazelversion` and
      `.ci/bazel-*.sha256`.
- [ ] Update all bazel rules dependencies in `WORKSPACE` (e.g.
      `io_bazel_stardoc`)
- [ ] Update bazel in nixpkgs and bump `nixpkgs/default.nix`
- [ ] Bump `MAX_BAZEL_*` in `start` and `haskell/private/versions.bzl`
- If we are updating to a new LTS:
  - Bump `MIN_BAZEL_*` in `start` and `haskell/private/versions.bzl`
  - TODO
- [ ] Add update notice to `CHANGELOG`
