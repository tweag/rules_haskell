"""Helper functions on lists."""

load("@bazel_skylib//lib:sets.bzl", "sets")

def _dedup_on(f, list_):
    """deduplicate `list_` by comparing the result of applying
    f to each element (e.g. comparing sub fields)

    def compare_x(el):
      return el.x

    dedup_on(compare_x, [struct(x=3), struct(x=4), struct(x=3)])
    => [struct(x=3), struct(x=4)]
    """
    seen = sets.make()
    deduped = []
    for el in list_:
        by = f(el)
        if not sets.contains(seen, by):
            sets.insert(seen, by)
            deduped.append(el)
    return deduped

list = struct(
    dedup_on = _dedup_on,
)
