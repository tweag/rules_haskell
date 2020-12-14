#!/usr/bin/env python3

from textwrap import dedent
import unittest

import package_configuration

class TestParsePackageConfiguration(unittest.TestCase):

    def _test_parse_package_configuration(self, lines, expected, msg=None):
        actual = package_configuration.parse_package_configuration(lines)
        self.assertEqual(actual, expected, msg)

    def test_parse_package_configuration_simple(self):
        lines = dedent("""\
            name:                 package
            version:              0.0.0.0
            id:                   package-0.0.0.0
            include-dirs:         /inc1 /inc2
            library-dirs:         /lib1 /lib2
            dynamic-library-dirs: /dylib1 /dylib2
            hs-libraries:         HSlib1 HSlib2
            depends:              dep1 dep2
            ld-options:           "-Wl,opt1" "-Wl,opt2"
            extra-libraries:      lib1 lib2
            haddock-interfaces:   /haddock1 /haddock2
            haddock-html:         /html

        """).splitlines()
        expected = package_configuration.PackageConfiguration(
            name = "package",
            version = "0.0.0.0",
            id = "package-0.0.0.0",
            include_dirs = ["/inc1", "/inc2"],
            library_dirs = ["/lib1", "/lib2"],
            dynamic_library_dirs = ["/dylib1", "/dylib2"],
            hs_libraries = ["HSlib1", "HSlib2"],
            depends = ["dep1", "dep2"],
            ld_options = ["-Wl,opt1", "-Wl,opt2"],
            extra_libraries = ["lib1", "lib2"],
            haddock_interfaces = ["/haddock1", "/haddock2"],
            haddock_html = "/html",
        )
        self._test_parse_package_configuration(lines, expected)

    def test_parse_package_configuration_wrapped(self):
        lines = dedent("""\
            name:
                package
            version:
                0.0.0.0
            id:
                package-0.0.0.0
            include-dirs:
                /inc1
                /inc2
            library-dirs:
                /lib1
                /lib2
            dynamic-library-dirs:
                /dylib1
                /dylib2
            hs-libraries:
                HSlib1
                HSlib2
            depends:
                dep1
                dep2
            ld-options:
                "-Wl,opt1"
                "-Wl,opt2"
            extra-libraries:
                lib1
                lib2
            haddock-interfaces:
                /haddock1
                /haddock2
            haddock-html:
                /html
        """).splitlines()
        expected = package_configuration.PackageConfiguration(
            name = "package",
            version = "0.0.0.0",
            id = "package-0.0.0.0",
            include_dirs = ["/inc1", "/inc2"],
            library_dirs = ["/lib1", "/lib2"],
            dynamic_library_dirs = ["/dylib1", "/dylib2"],
            hs_libraries = ["HSlib1", "HSlib2"],
            depends = ["dep1", "dep2"],
            ld_options = ["-Wl,opt1", "-Wl,opt2"],
            extra_libraries = ["lib1", "lib2"],
            haddock_interfaces = ["/haddock1", "/haddock2"],
            haddock_html = "/html",
        )
        self._test_parse_package_configuration(lines, expected)

class TestSplitRecords(unittest.TestCase):

    def _test_split_records(self, lines, expected, msg=None):
        actual = list(package_configuration.split_records(lines))
        self.assertEqual(actual, expected, msg)

    def test_empty(self):
        lines = "".splitlines()
        expected = []
        self._test_split_records(lines, expected)

    def test_single(self):
        lines = "x".splitlines()
        expected = [["x"]]
        self._test_split_records(lines, expected)

    def test_multiple(self):
        lines = "x1\nx2\n---\ny1\ny2\n---\nz1\nz2".splitlines()
        expected = [["x1", "x2"], ["y1", "y2"], ["z1", "z2"]]
        self._test_split_records(lines, expected)

class TestParsePackageDatabaseDump(unittest.TestCase):

    def _test_parse_package_database_dump(self, lines, expected, msg=None):
        actual = list(package_configuration.parse_package_database_dump(lines))
        self.assertEqual(actual, expected, msg)

    def test_regression_1453(self):
        """Regression test for https://github.com/tweag/rules_haskell/issues/1453

        Package db dump obtained using the following command on nixpkgs-21.03pre258014.2c0f6135aab
        ```
        $ nix-shell -p 'haskellPackages.ghcWithPackages (ps: [ps.data-default-instances-old-locale])' --run 'ghc-pkg dump --global'
        ```
        For brevity we only include the package `data-default-instances-old-locale` that triggered the issue plus two core packages.
        """
        lines = dedent("""\
            name:            rts
            version:         1.0
            visibility:      public
            id:              rts
            key:             rts
            license:         BSD-3-Clause
            maintainer:      glasgow-haskell-users@haskell.org
            exposed:         True
            library-dirs:
                /nix/store/4gz77sky04mdw54lcg7h9xpjnsd8873h-ghc-8.10.2/lib/ghc-8.10.2/rts
                /nix/store/pmy6g9dljads87bhg7q0lj08cyvsf13m-libffi-3.3/lib

            hs-libraries:    HSrts
            extra-libraries: m rt dl ffi pthread
            include-dirs:
                /nix/store/4gz77sky04mdw54lcg7h9xpjnsd8873h-ghc-8.10.2/lib/ghc-8.10.2/include
                /nix/store/i79avwy6757zlfwjkyawxfhsqm59pshm-libffi-3.3-dev/include

            includes:        Stg.h
            ld-options:
                "-Wl,-u,base_GHCziTopHandler_runIO_closure"
                "-Wl,-u,base_GHCziTopHandler_runNonIO_closure"
                "-Wl,-u,ghczmprim_GHCziTuple_Z0T_closure"
                "-Wl,-u,ghczmprim_GHCziTypes_True_closure"
                "-Wl,-u,ghczmprim_GHCziTypes_False_closure"
                "-Wl,-u,base_GHCziPack_unpackCString_closure"
                "-Wl,-u,base_GHCziWeak_runFinalizzerBatch_closure"
                "-Wl,-u,base_GHCziIOziException_stackOverflow_closure"
                "-Wl,-u,base_GHCziIOziException_heapOverflow_closure"
                "-Wl,-u,base_GHCziIOziException_allocationLimitExceeded_closure"
                "-Wl,-u,base_GHCziIOziException_blockedIndefinitelyOnMVar_closure"
                "-Wl,-u,base_GHCziIOziException_blockedIndefinitelyOnSTM_closure"
                "-Wl,-u,base_GHCziIOziException_cannotCompactFunction_closure"
                "-Wl,-u,base_GHCziIOziException_cannotCompactPinned_closure"
                "-Wl,-u,base_GHCziIOziException_cannotCompactMutable_closure"
                "-Wl,-u,base_ControlziExceptionziBase_absentSumFieldError_closure"
                "-Wl,-u,base_ControlziExceptionziBase_nonTermination_closure"
                "-Wl,-u,base_ControlziExceptionziBase_nestedAtomically_closure"
                "-Wl,-u,base_GHCziEventziThread_blockedOnBadFD_closure"
                "-Wl,-u,base_GHCziConcziSync_runSparks_closure"
                "-Wl,-u,base_GHCziConcziIO_ensureIOManagerIsRunning_closure"
                "-Wl,-u,base_GHCziConcziIO_ioManagerCapabilitiesChanged_closure"
                "-Wl,-u,base_GHCziConcziSignal_runHandlersPtr_closure"
                "-Wl,-u,base_GHCziTopHandler_flushStdHandles_closure"
                "-Wl,-u,base_GHCziTopHandler_runMainIO_closure"
                "-Wl,-u,ghczmprim_GHCziTypes_Czh_con_info"
                "-Wl,-u,ghczmprim_GHCziTypes_Izh_con_info"
                "-Wl,-u,ghczmprim_GHCziTypes_Fzh_con_info"
                "-Wl,-u,ghczmprim_GHCziTypes_Dzh_con_info"
                "-Wl,-u,ghczmprim_GHCziTypes_Wzh_con_info"
                "-Wl,-u,base_GHCziPtr_Ptr_con_info"
                "-Wl,-u,base_GHCziPtr_FunPtr_con_info"
                "-Wl,-u,base_GHCziInt_I8zh_con_info"
                "-Wl,-u,base_GHCziInt_I16zh_con_info"
                "-Wl,-u,base_GHCziInt_I32zh_con_info"
                "-Wl,-u,base_GHCziInt_I64zh_con_info"
                "-Wl,-u,base_GHCziWord_W8zh_con_info"
                "-Wl,-u,base_GHCziWord_W16zh_con_info"
                "-Wl,-u,base_GHCziWord_W32zh_con_info"
                "-Wl,-u,base_GHCziWord_W64zh_con_info"
                "-Wl,-u,base_GHCziStable_StablePtr_con_info"
                "-Wl,-u,hs_atomic_add8" "-Wl,-u,hs_atomic_add16"
                "-Wl,-u,hs_atomic_add32" "-Wl,-u,hs_atomic_add64"
                "-Wl,-u,hs_atomic_sub8" "-Wl,-u,hs_atomic_sub16"
                "-Wl,-u,hs_atomic_sub32" "-Wl,-u,hs_atomic_sub64"
                "-Wl,-u,hs_atomic_and8" "-Wl,-u,hs_atomic_and16"
                "-Wl,-u,hs_atomic_and32" "-Wl,-u,hs_atomic_and64"
                "-Wl,-u,hs_atomic_nand8" "-Wl,-u,hs_atomic_nand16"
                "-Wl,-u,hs_atomic_nand32" "-Wl,-u,hs_atomic_nand64"
                "-Wl,-u,hs_atomic_or8" "-Wl,-u,hs_atomic_or16"
                "-Wl,-u,hs_atomic_or32" "-Wl,-u,hs_atomic_or64"
                "-Wl,-u,hs_atomic_xor8" "-Wl,-u,hs_atomic_xor16"
                "-Wl,-u,hs_atomic_xor32" "-Wl,-u,hs_atomic_xor64"
                "-Wl,-u,hs_cmpxchg8" "-Wl,-u,hs_cmpxchg16" "-Wl,-u,hs_cmpxchg32"
                "-Wl,-u,hs_cmpxchg64" "-Wl,-u,hs_atomicread8"
                "-Wl,-u,hs_atomicread16" "-Wl,-u,hs_atomicread32"
                "-Wl,-u,hs_atomicread64" "-Wl,-u,hs_atomicwrite8"
                "-Wl,-u,hs_atomicwrite16" "-Wl,-u,hs_atomicwrite32"
                "-Wl,-u,hs_atomicwrite64"
            pkgroot: "/nix/store/0ixkkyd49s261i0ij60ny11y4q4sy56v-ghc-8.10.2-with-packages/lib/ghc-8.10.2"
            ---
            name:                 data-default-instances-old-locale
            version:              0.0.1
            visibility:           public
            id:
                data-default-instances-old-locale-0.0.1-LkrhLZM06G59SMNGVzL0si

            key:
                data-default-instances-old-locale-0.0.1-LkrhLZM06G59SMNGVzL0si

            license:              BSD-3-Clause
            maintainer:           <l.mai@web.de>
            author:               Lukas Mai
            synopsis:             Default instances for types in old-locale
            category:             Data
            abi:                  16f63d24fb87eab35a797c311fbce884
            exposed:              True
            exposed-modules:      Data.Default.Instances.OldLocale
            import-dirs:
                /nix/store/3qblwhql21nj10l12x3l1xgy3pihlswa-data-default-instances-old-locale-0.0.1/lib/ghc-8.10.2/x86_64-linux-ghc-8.10.2/data-default-instances-old-locale-0.0.1-LkrhLZM06G59SMNGVzL0si

            library-dirs:
                /nix/store/3qblwhql21nj10l12x3l1xgy3pihlswa-data-default-instances-old-locale-0.0.1/lib/ghc-8.10.2/x86_64-linux-ghc-8.10.2/data-default-instances-old-locale-0.0.1-LkrhLZM06G59SMNGVzL0si
                /nix/store/ba3mg4j6qmlqbf4k22ffl09y9vmv11rn-ncurses-6.2/lib
                /nix/store/pmy6g9dljads87bhg7q0lj08cyvsf13m-libffi-3.3/lib
                /nix/store/58iqzc5rrqgdczaivxja1zg1c1qaxix9-gmp-6.2.1/lib

            dynamic-library-dirs:
                /nix/store/3qblwhql21nj10l12x3l1xgy3pihlswa-data-default-instances-old-locale-0.0.1/lib/ghc-8.10.2/x86_64-linux-ghc-8.10.2
                /nix/store/ba3mg4j6qmlqbf4k22ffl09y9vmv11rn-ncurses-6.2/lib
                /nix/store/pmy6g9dljads87bhg7q0lj08cyvsf13m-libffi-3.3/lib
                /nix/store/58iqzc5rrqgdczaivxja1zg1c1qaxix9-gmp-6.2.1/lib

            data-dir:
                /nix/store/3qblwhql21nj10l12x3l1xgy3pihlswa-data-default-instances-old-locale-0.0.1/share/x86_64-linux-ghc-8.10.2/data-default-instances-old-locale-0.0.1

            hs-libraries:
                HSdata-default-instances-old-locale-0.0.1-LkrhLZM06G59SMNGVzL0si

            depends:
                base-4.14.1.0 data-default-class-0.1.2.0-IIN1s3V8yfYEDHe5yjxXHV
                old-locale-1.0.0.7-LpTuchyagHAP7STZdm71P

            haddock-interfaces:
                /nix/store/2nckrnr2pryxp4gin2i45h5fszwr80fx-data-default-instances-old-locale-0.0.1-doc/share/doc/data-default-instances-old-locale-0.0.1/html/data-default-instances-old-locale.haddock

            haddock-html:
                /nix/store/2nckrnr2pryxp4gin2i45h5fszwr80fx-data-default-instances-old-locale-0.0.1-doc/share/doc/data-default-instances-old-locale-0.0.1/html
            pkgroot: "/nix/store/0ixkkyd49s261i0ij60ny11y4q4sy56v-ghc-8.10.2-with-packages/lib/ghc-8.10.2"
            ---
            name:                 base
            version:              4.14.1.0
            visibility:           public
            id:                   base-4.14.1.0
            key:                  base-4.14.1.0
            license:              BSD-3-Clause
            maintainer:           libraries@haskell.org
            synopsis:             Basic libraries
            description:
                This package contains the Standard Haskell "Prelude" and its support libraries,
                and a large collection of useful libraries ranging from data
                structures to parsing combinators and debugging utilities.

            category:             Prelude
            abi:                  7722f39881285a6473d37342ad516c1b
            exposed:              True
            exposed-modules:
                Control.Applicative Control.Arrow Control.Category
                Control.Concurrent Control.Concurrent.Chan Control.Concurrent.MVar
                Control.Concurrent.QSem Control.Concurrent.QSemN Control.Exception
                Control.Exception.Base Control.Monad Control.Monad.Fail
                Control.Monad.Fix Control.Monad.IO.Class Control.Monad.Instances
                Control.Monad.ST Control.Monad.ST.Lazy Control.Monad.ST.Lazy.Safe
                Control.Monad.ST.Lazy.Unsafe Control.Monad.ST.Safe
                Control.Monad.ST.Strict Control.Monad.ST.Unsafe Control.Monad.Zip
                Data.Bifoldable Data.Bifunctor Data.Bitraversable Data.Bits
                Data.Bool Data.Char Data.Coerce Data.Complex Data.Data Data.Dynamic
                Data.Either Data.Eq Data.Fixed Data.Foldable Data.Function
                Data.Functor Data.Functor.Classes Data.Functor.Compose
                Data.Functor.Const Data.Functor.Contravariant Data.Functor.Identity
                Data.Functor.Product Data.Functor.Sum Data.IORef Data.Int Data.Ix
                Data.Kind Data.List Data.List.NonEmpty Data.Maybe Data.Monoid
                Data.Ord Data.Proxy Data.Ratio Data.STRef Data.STRef.Lazy
                Data.STRef.Strict Data.Semigroup Data.String Data.Traversable
                Data.Tuple Data.Type.Bool Data.Type.Coercion Data.Type.Equality
                Data.Typeable Data.Unique Data.Version Data.Void Data.Word
                Debug.Trace Foreign Foreign.C Foreign.C.Error Foreign.C.String
                Foreign.C.Types Foreign.Concurrent Foreign.ForeignPtr
                Foreign.ForeignPtr.Safe Foreign.ForeignPtr.Unsafe Foreign.Marshal
                Foreign.Marshal.Alloc Foreign.Marshal.Array Foreign.Marshal.Error
                Foreign.Marshal.Pool Foreign.Marshal.Safe Foreign.Marshal.Unsafe
                Foreign.Marshal.Utils Foreign.Ptr Foreign.Safe Foreign.StablePtr
                Foreign.Storable GHC.Arr GHC.Base GHC.ByteOrder GHC.Char GHC.Clock
                GHC.Conc GHC.Conc.IO GHC.Conc.Signal GHC.Conc.Sync
                GHC.ConsoleHandler GHC.Constants GHC.Desugar GHC.Enum
                GHC.Environment GHC.Err GHC.Event GHC.Exception GHC.Exception.Type
                GHC.ExecutionStack GHC.ExecutionStack.Internal GHC.Exts
                GHC.Fingerprint GHC.Fingerprint.Type GHC.Float
                GHC.Float.ConversionUtils GHC.Float.RealFracMethods GHC.Foreign
                GHC.ForeignPtr GHC.GHCi GHC.GHCi.Helpers GHC.Generics GHC.IO
                GHC.IO.Buffer GHC.IO.BufferedIO GHC.IO.Device GHC.IO.Encoding
                GHC.IO.Encoding.CodePage GHC.IO.Encoding.Failure
                GHC.IO.Encoding.Iconv GHC.IO.Encoding.Latin1 GHC.IO.Encoding.Types
                GHC.IO.Encoding.UTF16 GHC.IO.Encoding.UTF32 GHC.IO.Encoding.UTF8
                GHC.IO.Exception GHC.IO.FD GHC.IO.Handle GHC.IO.Handle.FD
                GHC.IO.Handle.Internals GHC.IO.Handle.Lock GHC.IO.Handle.Text
                GHC.IO.Handle.Types GHC.IO.IOMode GHC.IO.Unsafe GHC.IOArray
                GHC.IORef GHC.Int GHC.Ix GHC.List GHC.MVar GHC.Maybe GHC.Natural
                GHC.Num GHC.OldList GHC.OverloadedLabels GHC.Pack GHC.Profiling
                GHC.Ptr GHC.RTS.Flags GHC.Read GHC.Real GHC.Records
                GHC.ResponseFile GHC.ST GHC.STRef GHC.Show GHC.Stable
                GHC.StableName GHC.Stack GHC.Stack.CCS GHC.Stack.Types
                GHC.StaticPtr GHC.Stats GHC.Storable GHC.TopHandler GHC.TypeLits
                GHC.TypeNats GHC.Unicode GHC.Weak GHC.Word Numeric Numeric.Natural
                Prelude System.CPUTime System.Console.GetOpt System.Environment
                System.Environment.Blank System.Exit System.IO System.IO.Error
                System.IO.Unsafe System.Info System.Mem System.Mem.StableName
                System.Mem.Weak System.Posix.Internals System.Posix.Types
                System.Timeout Text.ParserCombinators.ReadP
                Text.ParserCombinators.ReadPrec Text.Printf Text.Read Text.Read.Lex
                Text.Show Text.Show.Functions Type.Reflection
                Type.Reflection.Unsafe Unsafe.Coerce

            hidden-modules:
                Control.Monad.ST.Imp Control.Monad.ST.Lazy.Imp Data.Functor.Utils
                Data.OldList Data.Semigroup.Internal Data.Typeable.Internal
                Foreign.ForeignPtr.Imp GHC.IO.Handle.Lock.Common
                GHC.IO.Handle.Lock.Flock GHC.IO.Handle.Lock.LinuxOFD
                GHC.IO.Handle.Lock.NoOp GHC.IO.Handle.Lock.Windows
                GHC.StaticPtr.Internal System.Environment.ExecutablePath
                System.CPUTime.Utils GHC.Event.Arr GHC.Event.Array
                GHC.Event.Control GHC.Event.EPoll GHC.Event.IntTable
                GHC.Event.Internal GHC.Event.KQueue GHC.Event.Manager GHC.Event.PSQ
                GHC.Event.Poll GHC.Event.Thread GHC.Event.TimerManager
                GHC.Event.Unique System.CPUTime.Posix.ClockGetTime
                System.CPUTime.Posix.Times System.CPUTime.Posix.RUsage
                System.CPUTime.Unsupported

            import-dirs:
                /nix/store/4gz77sky04mdw54lcg7h9xpjnsd8873h-ghc-8.10.2/lib/ghc-8.10.2/base-4.14.1.0

            library-dirs:
                /nix/store/4gz77sky04mdw54lcg7h9xpjnsd8873h-ghc-8.10.2/lib/ghc-8.10.2/base-4.14.1.0

            dynamic-library-dirs:
                /nix/store/4gz77sky04mdw54lcg7h9xpjnsd8873h-ghc-8.10.2/lib/ghc-8.10.2/base-4.14.1.0

            data-dir:
                /nix/store/4gz77sky04mdw54lcg7h9xpjnsd8873h-ghc-8.10.2/share/x86_64-linux-ghc-8.10.2/base-4.14.1.0

            hs-libraries:         HSbase-4.14.1.0
            include-dirs:
                /nix/store/4gz77sky04mdw54lcg7h9xpjnsd8873h-ghc-8.10.2/lib/ghc-8.10.2/base-4.14.1.0/include

            includes:             HsBase.h
            depends:              ghc-prim-0.6.1 integer-gmp-1.0.3.0 rts
            haddock-interfaces:
                /nix/store/d5iig5kqvalfsiqkzbpq33avyia2k2fi-ghc-8.10.2-doc/share/doc/ghc/html/libraries/base-4.14.1.0/base.haddock

            haddock-html:
                /nix/store/d5iig5kqvalfsiqkzbpq33avyia2k2fi-ghc-8.10.2-doc/share/doc/ghc/html/libraries/base-4.14.1.0
            pkgroot: "/nix/store/0ixkkyd49s261i0ij60ny11y4q4sy56v-ghc-8.10.2-with-packages/lib/ghc-8.10.2"
        """).splitlines()
        expected = [
            package_configuration.PackageConfiguration(
                name = "rts",
                version = "1.0",
                id = "rts",
                include_dirs = [
                    "/nix/store/4gz77sky04mdw54lcg7h9xpjnsd8873h-ghc-8.10.2/lib/ghc-8.10.2/include",
                    "/nix/store/i79avwy6757zlfwjkyawxfhsqm59pshm-libffi-3.3-dev/include",
                ],
                library_dirs = [
                    "/nix/store/4gz77sky04mdw54lcg7h9xpjnsd8873h-ghc-8.10.2/lib/ghc-8.10.2/rts",
                    "/nix/store/pmy6g9dljads87bhg7q0lj08cyvsf13m-libffi-3.3/lib",
                ],
                dynamic_library_dirs = [],
                hs_libraries = ["HSrts"],
                depends = [],
                ld_options = [
                    "-Wl,-u,base_GHCziTopHandler_runIO_closure",
                    "-Wl,-u,base_GHCziTopHandler_runNonIO_closure",
                    "-Wl,-u,ghczmprim_GHCziTuple_Z0T_closure",
                    "-Wl,-u,ghczmprim_GHCziTypes_True_closure",
                    "-Wl,-u,ghczmprim_GHCziTypes_False_closure",
                    "-Wl,-u,base_GHCziPack_unpackCString_closure",
                    "-Wl,-u,base_GHCziWeak_runFinalizzerBatch_closure",
                    "-Wl,-u,base_GHCziIOziException_stackOverflow_closure",
                    "-Wl,-u,base_GHCziIOziException_heapOverflow_closure",
                    "-Wl,-u,base_GHCziIOziException_allocationLimitExceeded_closure",
                    "-Wl,-u,base_GHCziIOziException_blockedIndefinitelyOnMVar_closure",
                    "-Wl,-u,base_GHCziIOziException_blockedIndefinitelyOnSTM_closure",
                    "-Wl,-u,base_GHCziIOziException_cannotCompactFunction_closure",
                    "-Wl,-u,base_GHCziIOziException_cannotCompactPinned_closure",
                    "-Wl,-u,base_GHCziIOziException_cannotCompactMutable_closure",
                    "-Wl,-u,base_ControlziExceptionziBase_absentSumFieldError_closure",
                    "-Wl,-u,base_ControlziExceptionziBase_nonTermination_closure",
                    "-Wl,-u,base_ControlziExceptionziBase_nestedAtomically_closure",
                    "-Wl,-u,base_GHCziEventziThread_blockedOnBadFD_closure",
                    "-Wl,-u,base_GHCziConcziSync_runSparks_closure",
                    "-Wl,-u,base_GHCziConcziIO_ensureIOManagerIsRunning_closure",
                    "-Wl,-u,base_GHCziConcziIO_ioManagerCapabilitiesChanged_closure",
                    "-Wl,-u,base_GHCziConcziSignal_runHandlersPtr_closure",
                    "-Wl,-u,base_GHCziTopHandler_flushStdHandles_closure",
                    "-Wl,-u,base_GHCziTopHandler_runMainIO_closure",
                    "-Wl,-u,ghczmprim_GHCziTypes_Czh_con_info",
                    "-Wl,-u,ghczmprim_GHCziTypes_Izh_con_info",
                    "-Wl,-u,ghczmprim_GHCziTypes_Fzh_con_info",
                    "-Wl,-u,ghczmprim_GHCziTypes_Dzh_con_info",
                    "-Wl,-u,ghczmprim_GHCziTypes_Wzh_con_info",
                    "-Wl,-u,base_GHCziPtr_Ptr_con_info",
                    "-Wl,-u,base_GHCziPtr_FunPtr_con_info",
                    "-Wl,-u,base_GHCziInt_I8zh_con_info",
                    "-Wl,-u,base_GHCziInt_I16zh_con_info",
                    "-Wl,-u,base_GHCziInt_I32zh_con_info",
                    "-Wl,-u,base_GHCziInt_I64zh_con_info",
                    "-Wl,-u,base_GHCziWord_W8zh_con_info",
                    "-Wl,-u,base_GHCziWord_W16zh_con_info",
                    "-Wl,-u,base_GHCziWord_W32zh_con_info",
                    "-Wl,-u,base_GHCziWord_W64zh_con_info",
                    "-Wl,-u,base_GHCziStable_StablePtr_con_info",
                    "-Wl,-u,hs_atomic_add8",
                    "-Wl,-u,hs_atomic_add16",
                    "-Wl,-u,hs_atomic_add32",
                    "-Wl,-u,hs_atomic_add64",
                    "-Wl,-u,hs_atomic_sub8",
                    "-Wl,-u,hs_atomic_sub16",
                    "-Wl,-u,hs_atomic_sub32",
                    "-Wl,-u,hs_atomic_sub64",
                    "-Wl,-u,hs_atomic_and8",
                    "-Wl,-u,hs_atomic_and16",
                    "-Wl,-u,hs_atomic_and32",
                    "-Wl,-u,hs_atomic_and64",
                    "-Wl,-u,hs_atomic_nand8",
                    "-Wl,-u,hs_atomic_nand16",
                    "-Wl,-u,hs_atomic_nand32",
                    "-Wl,-u,hs_atomic_nand64",
                    "-Wl,-u,hs_atomic_or8",
                    "-Wl,-u,hs_atomic_or16",
                    "-Wl,-u,hs_atomic_or32",
                    "-Wl,-u,hs_atomic_or64",
                    "-Wl,-u,hs_atomic_xor8",
                    "-Wl,-u,hs_atomic_xor16",
                    "-Wl,-u,hs_atomic_xor32",
                    "-Wl,-u,hs_atomic_xor64",
                    "-Wl,-u,hs_cmpxchg8",
                    "-Wl,-u,hs_cmpxchg16",
                    "-Wl,-u,hs_cmpxchg32",
                    "-Wl,-u,hs_cmpxchg64",
                    "-Wl,-u,hs_atomicread8",
                    "-Wl,-u,hs_atomicread16",
                    "-Wl,-u,hs_atomicread32",
                    "-Wl,-u,hs_atomicread64",
                    "-Wl,-u,hs_atomicwrite8",
                    "-Wl,-u,hs_atomicwrite16",
                    "-Wl,-u,hs_atomicwrite32",
                    "-Wl,-u,hs_atomicwrite64",
                ],
                extra_libraries = ["m", "rt", "dl", "ffi", "pthread"],
                haddock_interfaces = [],
                haddock_html = None,
            ),
            package_configuration.PackageConfiguration(
                name = "data-default-instances-old-locale",
                version = "0.0.1",
                id = "data-default-instances-old-locale-0.0.1-LkrhLZM06G59SMNGVzL0si",
                include_dirs = [],
                library_dirs = [
                    "/nix/store/3qblwhql21nj10l12x3l1xgy3pihlswa-data-default-instances-old-locale-0.0.1/lib/ghc-8.10.2/x86_64-linux-ghc-8.10.2/data-default-instances-old-locale-0.0.1-LkrhLZM06G59SMNGVzL0si",
                    "/nix/store/ba3mg4j6qmlqbf4k22ffl09y9vmv11rn-ncurses-6.2/lib",
                    "/nix/store/pmy6g9dljads87bhg7q0lj08cyvsf13m-libffi-3.3/lib",
                    "/nix/store/58iqzc5rrqgdczaivxja1zg1c1qaxix9-gmp-6.2.1/lib",
                ],
                dynamic_library_dirs = [
                    "/nix/store/3qblwhql21nj10l12x3l1xgy3pihlswa-data-default-instances-old-locale-0.0.1/lib/ghc-8.10.2/x86_64-linux-ghc-8.10.2",
                    "/nix/store/ba3mg4j6qmlqbf4k22ffl09y9vmv11rn-ncurses-6.2/lib",
                    "/nix/store/pmy6g9dljads87bhg7q0lj08cyvsf13m-libffi-3.3/lib",
                    "/nix/store/58iqzc5rrqgdczaivxja1zg1c1qaxix9-gmp-6.2.1/lib",
                ],
                hs_libraries = ["HSdata-default-instances-old-locale-0.0.1-LkrhLZM06G59SMNGVzL0si"],
                depends = [
                    "base-4.14.1.0",
                    "data-default-class-0.1.2.0-IIN1s3V8yfYEDHe5yjxXHV",
                    "old-locale-1.0.0.7-LpTuchyagHAP7STZdm71P",
                ],
                ld_options = [],
                extra_libraries = [],
                haddock_interfaces = [
                    "/nix/store/2nckrnr2pryxp4gin2i45h5fszwr80fx-data-default-instances-old-locale-0.0.1-doc/share/doc/data-default-instances-old-locale-0.0.1/html/data-default-instances-old-locale.haddock",
                ],
                haddock_html = "/nix/store/2nckrnr2pryxp4gin2i45h5fszwr80fx-data-default-instances-old-locale-0.0.1-doc/share/doc/data-default-instances-old-locale-0.0.1/html",
            ),
            package_configuration.PackageConfiguration(
                name = "base",
                version = "4.14.1.0",
                id = "base-4.14.1.0",
                include_dirs = [
                    "/nix/store/4gz77sky04mdw54lcg7h9xpjnsd8873h-ghc-8.10.2/lib/ghc-8.10.2/base-4.14.1.0/include",
                ],
                library_dirs = [
                    "/nix/store/4gz77sky04mdw54lcg7h9xpjnsd8873h-ghc-8.10.2/lib/ghc-8.10.2/base-4.14.1.0",
                ],
                dynamic_library_dirs = [
                    "/nix/store/4gz77sky04mdw54lcg7h9xpjnsd8873h-ghc-8.10.2/lib/ghc-8.10.2/base-4.14.1.0",
                ],
                hs_libraries = ["HSbase-4.14.1.0"],
                depends = [
                    "ghc-prim-0.6.1",
                    "integer-gmp-1.0.3.0",
                    "rts",
                ],
                ld_options = [],
                extra_libraries = [],
                haddock_interfaces = [
                    "/nix/store/d5iig5kqvalfsiqkzbpq33avyia2k2fi-ghc-8.10.2-doc/share/doc/ghc/html/libraries/base-4.14.1.0/base.haddock",
                ],
                haddock_html = "/nix/store/d5iig5kqvalfsiqkzbpq33avyia2k2fi-ghc-8.10.2-doc/share/doc/ghc/html/libraries/base-4.14.1.0",
            ),
        ]
        self._test_parse_package_database_dump(lines, expected)

if __name__ == "__main__":
    unittest.main()
