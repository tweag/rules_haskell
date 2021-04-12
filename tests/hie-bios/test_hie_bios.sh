#!/bin/sh

# To test the hie_bios_path_prefix argument of haskell_repl rule.
# we check that the prefix $magic_string is added to some paths of file $1
# and that if we remove them, we get back file $2

if grep -q '$magic_string' "$1"; then
    sed 's#$magic_string/##' "$1" | cmp "$2"
else
    echo "\$magic_string should be in file: $1"
    exit 1
fi
