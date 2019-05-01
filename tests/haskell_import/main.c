#include <stdio.h>
#include "HsFFI.h"

extern HsInt add_one_hs(HsInt a0);

int main(int argc, char *argv[]) {
  hs_init(&argc, &argv);
  printf("Adding one to 5 through Haskell is %ld\n", add_one_hs(5));
  hs_exit();
  return 0;
}
