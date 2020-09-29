module Code (intfun, boolfun) where

#include <code.h>

{#fun intfun {`Int'} -> `Int'#}
{#fun boolfun {`Int'} -> `Bool'#}
