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

- [ ] Create a [new issue][release-issue] from the "Prepare new release"
      template and follow the instructions there.

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
[release-issue]: https://github.com/tweag/rules_haskell/issues/new?template=release.md
