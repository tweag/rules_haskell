module Foo.Foo (foo) where

#include <zlib.h>

foo :: Int
foo = {# sizeof gz_header_s #}
