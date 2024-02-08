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
- [ ] Set the revision in [the `start` script][start] and
      [`docs/haskell-use-cases`][usecases] to the current release
      preparation branch; comment out the checksum. (n.b., Search for
      `http_archive` in these files.)
- [ ] Update the version of the modules in `MODULE.bazel` files
- [ ] Push the `release-<major>.<minor>` branch and open a **draft** PR
      to verify CI is green.
- [ ] Create a release tag (`v<major>.<minor>`) on the release
      preparation branch and push the tag; or use Github's UI.
- [ ] Go to the [release page][releases]:
    - [ ] Open the corresponding draft release and copy the workspace snippet.
    - [ ] Insert the workspace snippet into [the `start` script][start]
          and [`docs/haskell-use-cases`][usecases] replacing the existing snippet. 
- [ ] Push the changes to the remote branch and mark the PR as ready;
      go through review and merge to `master` upon success.
  - If any changes need to be made, upon review, you will need to delete
    the release tag (from local and origin) and repeat the previous four
    steps appropriately before requesting a follow-up review.
  - If there are changes on the release preparation branch that should
    *not* go to `master`, create a second branch
    `release-<major>.<minor>-master` on `master` and cherry-pick all
    relevant commits from the release branch preparation branch. Open a
    pull request with that branch, go through review and push changes
    back to the release preparation branch.
- [ ] Go to the [release page][releases]:
    - [ ] Open the draft release for the current version.
    - [ ] Release.
- [ ] After the "Publish" workflow is finished check whether https://haskell.build/start
      is now the latest [`start` script][start] (Netlify sometimes has problems).
- [ ] Announce the new version on Twitter by asking someone with access.


[start]: /start
[versions]: /haskell/private/versions.bzl
[changelog]: /CHANGELOG.md
[usecases]: /docs/haskell-use-cases.rst
[readme]: /README.md
[releases]: https://github.com/tweag/rules_haskell/releases
