"""Helper functions on lists."""

load(":private/set.bzl", "set")

def _dedup_on(f, list_):
    """deduplicate `list_` by comparing the result of applying
    f to each element (e.g. comparing sub fields)

    def compare_x(el):
      return el.x

    dedup_on([struct(x=3), struct(x=4), struct(x=3)], compare_x)
    => [struct(x=3), struct(x=4)]
    """
    seen = set.empty()
    deduped = []
    for el in list_:
        by = f(el)
        if not set.is_member(seen, by):
            set.mutable_insert(seen, by)
            deduped.append(el)
    return deduped

list = struct(
    dedup_on = _dedup_on,
)
