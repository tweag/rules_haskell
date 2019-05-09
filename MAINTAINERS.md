# Maintenance instructions

## Cutting a new release

- Copy this list of steps into a `rules_haskell` issue
- Create & checkout release branch (`release-<major>.<minor>`).
- [ ] Fix minimal bazel version in [the `start` script](./start) and
  [the README](./README.md), add to the [`CHANGELOG`](./CHANGELOG.md)
  if it changed.
- [ ] Remove any feature that is still too experimental to go into a
  release, by cherry-picking reverts (or by manually deleting the
  feature).
- [ ] Write `CHANGELOG` by going through all pull requests since last
  release (https://github.com/tweag/rules_haskell/pulls?q=is%3Apr).
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
  push
- [ ] Go to the [release
  page](https://github.com/tweag/rules_haskell/releases)
    - click on “Draft a new release”
    - name `v<major>.<minor>`
    - copy the changelog section into the description
    - release
- [ ] Push `rules_haskell` version in start script to new release tag,
  test it in a temporary directory, create PR against master, publish
  start script.
- [ ] Announce the new version (on Twitter)
