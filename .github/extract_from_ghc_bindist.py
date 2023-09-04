import os
import sys

current_dir = os.path.dirname( __file__ )
haskell_dir = os.path.join( current_dir, '..', 'haskell')
sys.path.append( haskell_dir )

import gen_ghc_bindist

def unexpected(unexpected, l, error_message):
  for a in unexpected:
    if a in l:
      print(error_message, file=sys.stderr)
      list.remove(a)

version_numbers = [x['version'] for x in gen_ghc_bindist.VERSIONS]

unexpected(["8.10.1", "8.10.2"], version_numbers, "GHC 8.10.1 and 8.10.2 not supported. Upgrade to 8.10.3 or later.")

with open(os.environ['GITHUB_OUTPUT'], mode='a', encoding='utf-8') as output:
  output.write("ghc-matrix={}\n".format(version_numbers))

