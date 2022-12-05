# Maintenance Instructions

## Experimental Features

When adding experimental features, your PR should include them in the
[list](#remove-these-prs-from-minor-releases) at the end of this
document if you don't feel they are yet ready for release.

## Bumping the Bazel Version

`rules_haskell` should always support the latest LTS release of Bazel.

We strive to always test against the latest non-LTS release nonetheless,
so bumping Bazel regularly is required.

- [ ] Use `.ci/update-bazel-version` to update `.ci/bazelversion` and
      `.ci/bazel-*.sha256`.
- [ ] Update all Bazel rules dependencies in `WORKSPACE` (e.g.,
      `io_bazel_stardoc`).
- [ ] Update Bazel in nixpkgs and bump `nixpkgs/default.nix`.
- [ ] Bump `MAX_BAZEL_*` in [the `start` script][start] and
      [`haskell/private/versions.bzl`][versions].
- [ ] If we are updating to a new LTS, bump `MIN_BAZEL_*` in
      [`start`][start] and [`haskell/private/versions.bzl`][versions].
- [ ] Add update notice to the [`CHANGELOG`][changelog].

## Cutting a New Release

- [ ] Read through this process in its entirety so you understand it.
- [ ] Copy this list of steps into an empty `rules_haskell` issue.
- [ ] Create and checkout a new release preparation branch, named
      `release-<major>.<minor>`.
- [ ] Update the minimal Bazel version in [the `start` script][start],
      [`haskell/private/versions.bzl`][versions], and [the
      `README`][readme]; add it to the [`CHANGELOG`][changelog] if it
      changed.
- [ ] Remove any feature that is still too experimental to go into a
      release, by cherry-picking reverts (or by manually deleting the
      feature).
  - [ ] Check the list [below](#remove-these-prs-from-minor-releases)
        for PRs that have been explicitly marked for removal, if any.
- [ ] Amend the [`CHANGELOG`][changelog] by summarising all significant
      pull requests since the last release (see
      [below](#generating-the-pr-list-for-the-changelog)). Specifically:
  - [ ] Add a "Highlights" section for major improvements/changes.
  - [ ] Create "Added", "Removed", "Changed" and "Fixed" sections, as
        necessary.
  - [ ] If relevant, add links to the corresponding PRs to the entries.
- [ ] Set the revision in [the `start` script][start] and
      [`docs/haskell-use-cases`][usecases] to the current release
      preparation branch; comment out the checksum. (n.b., Search for
      `http_archive` in these files.)
- [ ] Push the `release-<major>.<minor>` branch and open a **draft** PR
      to verify CI is green.
- [ ] Create a release tag (`v<major>.<minor>`) on the release
      preparation branch and push the tag or use Github's UI.
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
- [ ] Merge `master` into the `release` branch and push to trigger
      deployment.
  - [ ] Check whether https://haskell.build/start is now the latest
        [`start` script][start] (Netlify sometimes has problems).
- [ ] Announce the new version on Twitter by asking someone with access.

### Generating the PR List for the CHANGELOG

The list of merged PRs can be fetched from:

    https://github.com/tweag/rules_haskell/pulls?q=is:pr+base:master+merged:>YYYY-MM-DD

Replacing `YYYY-MM-DD` by the date of the last release.

If you have the GitHub CLI client, the following may be more convenient:

```bash
gh pr list -L 500 -B master -s merged \
           --json number,mergedAt,title,body \
| jq -r --argjson release "$(gh release view --json createdAt)" '
     reverse | .[] | select(.mergedAt > $release.createdAt) |
     ["# PR#\(.number): \(.title)", "*Merged: \(.mergedAt)*", "\(.body)\n"] |
     join("\n\n")' \
> PRs.md
```

**Note** The `-L 500` is an arbitrary "large number" limit of PRs to
fetch, overriding the low default. As of writing, there's no way to set
this to "unlimited"; adjust as necessary.

### Generating the Archive Checksum

The Git archive checksum can be calculated with:

```bash
REV=0.x
git archive --format=tar.gz --prefix=rules_haskell-${REV}/ v${REV} | sha256sum
```

**Note** The trailing slash on the prefix is important; don't forget it.

**Note** GitHub normalises release tags in the archive prefix to exclude
the initial "v"; adjust as necessary.

### Remove these PRs from Minor Releases

- None

<!-- Links -->
[start]: ./start
[versions]: ./haskell/private/versions.bzl
[readme]: ./README.md
[changelog]: ./CHANGELOG.md
[usecases]: ./docs/haskell-use-cases.rst
[releases]: https://github.com/tweag/rules_haskell/releases
