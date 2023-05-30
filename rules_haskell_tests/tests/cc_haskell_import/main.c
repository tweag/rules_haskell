#include <stdio.h>
#include "HsFFI.h"

extern HsInt32 add_one_hs(HsInt32 a0);

int main(int argc, char *argv[]) {
  hs_init(&argc, &argv);
  printf("Adding one to 5 through Haskell is %d\n", add_one_hs(5));
  hs_exit();
  return 0;
}
