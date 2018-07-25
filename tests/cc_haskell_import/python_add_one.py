import ctypes

foreignlib = ctypes.cdll.LoadLibrary('tests/cc_haskell_import/libadd_one.so')
print(foreignlib)
foreignlib.hs_init()
print('one plus one equals: ' + str(foreignlib.add_one_hs(1)))
print('success!')
