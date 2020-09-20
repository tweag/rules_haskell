module Final where

import Intermediate
import Root

final :: ()
final = intermediate (Root ())
