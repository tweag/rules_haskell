#!/usr/bin/env python

import os
import importlib
import importlib.util
import importlib.machinery
import urllib.request
import sys
import hashlib
import tarfile
import tempfile
from pathlib import PurePosixPath, Path
from subprocess import Popen, PIPE, STDOUT, DEVNULL, check_call


def calculate_sha256(file_path):
    sha256_hash = hashlib.sha256()

    with open(file_path, "rb") as file:
        # Read the file in chunks to handle large files efficiently
        chunk_size = 4096
        chunk = file.read(chunk_size)
        while len(chunk) > 0:
            sha256_hash.update(chunk)
            chunk = file.read(chunk_size)

    return sha256_hash.hexdigest()

def import_path(path):
    module_name = os.path.basename(path).replace('-', '_')
    spec = importlib.util.spec_from_loader(
        module_name,
        importlib.machinery.SourceFileLoader(module_name, path)
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    sys.modules[module_name] = module
    return module

def need_to_download(file_path, sha256_hash):
    if not os.path.exists(file_path):
        return True
    else:
        return calculate_sha256(file_path).upper() != sha256_hash.upper()

def download_file(url, file_path):
    urllib.request.urlretrieve(url, file_path)

def main():
    if len(sys.argv) != 2:
        print("usage: gen-patch-win.py GHC_VERSION", file=sys.stderr)
        sys.exit(1)

    VERSION = sys.argv[1]

    # Download the ghc version for windows from the URL given in ghc_bindist_generated.bzl

    SCRIPT_PATH = Path(__file__)
    bindist = import_path(str(SCRIPT_PATH.parent.parent.joinpath('private/ghc_bindist_generated.bzl')))

    ghc_url, hash = bindist.GHC_BINDIST[VERSION]['windows_amd64']

    file_path = f'ghc-{VERSION}.windows_amd64.tar.xz'

    if need_to_download(file_path, hash):
        print("downloading...", ghc_url, file=sys.stderr)
        download_file(ghc_url, file_path)

        assert not need_to_download(file_path, hash), "file disappeared or hash mismatch"
    else:
        print("found existing file", file_path, file=sys.stderr)

    print('extracting...', file=sys.stderr)

    with tarfile.open(file_path, "r:xz") as tar:
        temp_dir = tempfile.mkdtemp()

        for member in tar.getmembers():
            member_path = PurePosixPath(member.name)
            if member_path.match('*/lib/package.conf.d/*'):
                tar.extract(member, path=temp_dir)
                extracted_path = Path(temp_dir, member_path)
                stripped_path = Path(temp_dir, *member_path.parts[1:])

                if not member.isdir():
                    # print(extracted_path, '->', stripped_path)
                    stripped_path.parent.mkdir(parents=True, exist_ok=True)
                    extracted_path.rename(stripped_path)

        check_call(['git', 'init'], cwd=temp_dir, stdout=DEVNULL)
        check_call(['git', 'add', '.'], cwd=temp_dir, stdout=DEVNULL)
        check_call(['git', 'config', 'user.email', 'no@body.com'], cwd=temp_dir)
        check_call(['git', 'config', 'user.name', 'user'], cwd=temp_dir)
        check_call(['git', 'commit', '-m', 'packages'], cwd=temp_dir, stdout=DEVNULL)

        # Modify the paths relative to `${pkgroot}`

        package_d = Path(temp_dir, 'lib/package.conf.d')

        config_files = [ f for f in package_d.iterdir() if f.suffix == '.conf' ]

        check_call(['sed', '-i', r's/{pkgroot}\/\.\.\/\.\.\//{pkgroot}\/\.\.\//g'] + config_files, cwd=package_d)

        # Generate the patch

        dashed_version = VERSION.replace('.', '_')
        patch_path = SCRIPT_PATH.parent.joinpath(f'ghc_{dashed_version}_win.patch')

        print('writing patch to', patch_path, file=sys.stderr)

        with Popen(['git', 'diff', '--no-prefix'], cwd=temp_dir, stdout=PIPE, text=True) as p:
            with patch_path.open('w') as f:
                for line in p.stdout:
                    # skip parasitic lines
                    if line.startswith('diff '): continue
                    if line.startswith('index '): continue

                    f.write(line)


if __name__ == '__main__':
    main()
