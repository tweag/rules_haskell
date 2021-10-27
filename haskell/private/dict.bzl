"""Helper functions on dicts."""

def find(d, value):
    """ Look for the first key correspondind to value `value` in dictionnary `d` """
    for (k, v) in d.items():
        if v == value:
            return k
