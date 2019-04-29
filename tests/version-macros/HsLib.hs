{-# LANGUAGE CPP #-}

module HsLib where

import Control.Monad (unless)

check_version_versioned_lib :: IO ()
#ifndef VERSION_versioned_lib
check_version_versioned_lib = fail "hs: VERSION_versioned_lib missing"
#else
check_version_versioned_lib =
  unless (VERSION_versioned_lib == "1.2.3.4") $
    fail "hs: VERSION_versioned_lib invalid"
#endif

check_min_version_versioned_lib :: IO ()
#ifndef MIN_VERSION_versioned_lib
check_min_version_versioned_lib = fail "hs: MIN_VERSION_versioned_lib missing"
#elif !MIN_VERSION_versioned_lib(1,2,3)
check_min_version_versioned_lib = fail "hs: MIN_VERSION_versioned_lib invalid"
#else
check_min_version_versioned_lib = pure ()
#endif

check_version_base :: IO ()
#ifndef VERSION_base
check_version_base = fail "hs: VERSION_base missing"
#else
check_version_base = pure ()
#endif

check_min_version_base :: IO ()
#ifndef MIN_VERSION_base
check_min_version_base = fail "hs: MIN_VERSION_base missing"
#elif !MIN_VERSION_base(0,0,0)
check_min_version_base = fail "hs: MIN_VERSION_base invalid"
#else
check_min_version_base = pure ()
#endif

check :: IO ()
check = do
  check_version_versioned_lib
  check_min_version_versioned_lib
  check_version_base
  check_min_version_base
