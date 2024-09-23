---
name: Prepare new release (for maintainers only)
title: Prepare release MAJOR.MINOR
about: Steps to work through in order to publish a new release

---

- [ ] Read through this process in its entirety so you understand it.
- [ ] Create and checkout a new release preparation branch, named
      `release-<major>.<minor>`.
- [ ] If the minimal Bazel version has changed:
  - [ ] update it in [the `start` script][start], in [`haskell/private/versions.bzl`][versions], in [the `README`][readme] and in `presubmit.yml` files from the `.bcr` folder
  - [ ] add a note about this change to the [`CHANGELOG`][changelog]
- [ ] Remove any feature that is still too experimental to go into a
      release, by cherry-picking reverts (or by manually deleting the
      feature).
  - [ ] Check the list [here](/MAINTAINERS.md#remove-these-prs-from-minor-releases)
        for PRs that have been explicitly marked for removal, if any.
- [ ] Amend the [`CHANGELOG`][changelog] by summarising all significant
      pull requests since the last release (see
      [here](/MAINTAINERS.md#generating-the-pr-list-for-the-changelog)). Specifically:
  - [ ] Add a "Highlights" section for major improvements/changes.
  - [ ] Create "Added", "Removed", "Changed" and "Fixed" sections, as
        necessary.
  - [ ] If relevant, add links to the corresponding PRs to the entries.
- [ ] Update the version of the modules in `MODULE.bazel` files
- [ ] Push the `release-<major>.<minor>` branch and open a PR; go through review and merge upon success.
- [ ] Trigger the `Prepare Release` workflow
  - either via the Github UI **or**
  - run `gh workflow run -f version=<major>.<minor> 'Prepare Release'` using the Github CLI
- [ ] Go to the [releases], open the draft release which was created to inspect it
  - Do the code snippets look valid?
  - Is there a release artifact attached to it?
  - If you're happy, publish the release... :rocket:
- [ ] After the "Publish" workflow is finished check whether https://haskell.build/start
      is now the latest [`start` script][start] (Netlify sometimes has problems).
- [ ] Announce the new version on Twitter by asking someone with access.


[start]: /start
[versions]: /haskell/private/versions.bzl
[changelog]: /CHANGELOG.md
[usecases]: /docs/haskell-use-cases.rst
[readme]: /README.md
[releases]: https://github.com/tweag/rules_haskell/releases
