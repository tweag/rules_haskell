module Baz (baz) where

#include <zlib.h>

baz :: Int
baz = {# sizeof gz_header_s #}
