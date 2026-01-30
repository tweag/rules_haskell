"""Helper functions on dicts."""

def find(d, value):
    """ Look for the first key corresponding to value `value` in dictionary `d` """
    for (k, v) in d.items():
        if v == value:
            return k
    return None
