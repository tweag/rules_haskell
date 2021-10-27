import json
import sys
from subprocess import (Popen, PIPE)
import ast
import re

def normalise_os(os):
    """ recognise os name using aliases known to cabal """
    os = os.lower()
    if os in ["mingw32", "win32", "cygwin32"]: return "windows"
    if os == "darwin": return "osx"
    if os == "gnu": return "hurd"
    if os == "kfreebsdgnu": return "freebsd"
    if os == "solaris2": return "solaris"
    if os in ["linux-android", "linux-androideabi", "linux-androideabihf"]:
        return "android"
    return os

def normalise_arch(arch):
    """ recognise architecture name using aliases known to cabal """
    arch = arch.lower()
    if arch == "powerpc": return "ppc"
    if arch in ["powerpc64", "powerpc64le"]: return "ppc64"
    if arch in ["sparc64", "sun4"]: return "sparc"
    if arch in ["mipsel", "mipseb"]: return "mips"
    if arch in ["armeb", "armel"]: return "arm"
    if arch == "arm64": return "aarch64"
    return arch

def platform_info(ghc):
    p = Popen(f"{ghc} --info", shell=True, stdout=PIPE)
    ghc_info_string = p.stdout.read().decode("utf-8")
    ghc_info = ast.literal_eval("("+ghc_info_string+")")
    target_arch = None
    target_os = None
    host_arch = None
    host_os = None
    for (k, v) in ghc_info:
        if k == "Target platform":
            m = re.match("([^-]*)-[^-]*-([^-]*)", v, re.IGNORECASE)
            if m:
                target_arch = normalise_arch(m.group(1))
                target_os = normalise_os(m.group(2))

        if k == "Host platform":
            m = re.match("([^-]*)-[^-]*-([^-]*)", v, re.IGNORECASE)
            if m:
                host_arch = normalise_arch(m.group(1))
                host_os = normalise_os(m.group(2))

    return {
        "target_arch": target_arch,
        "target_os": target_os,
        "host_arch": host_arch,
        "host_os": host_os,
    }

if __name__ == "__main__":
    ghc_path = sys.argv[1]
    info = platform_info(ghc_path)
    # Prints the parsed info as json to be understable by bazel.
    print(json.dumps(info))
