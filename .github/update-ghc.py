#!/usr/bin/env python

import os, sys, re
import importlib
import importlib.util
import importlib.machinery
import http.client
import json
from pathlib import PurePosixPath, Path
from pprint import pprint
from urllib.parse import quote
from subprocess import Popen, PIPE, STDOUT, DEVNULL, check_output, run

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


GHC_GITLAB_SERVER="gitlab.haskell.org"

def get_gitlab_tags_with_prefix(project_id, prefix):
    connection = http.client.HTTPSConnection(GHC_GITLAB_SERVER)
    encoded_project_id = encoded_project_id = quote(project_id, safe='')
    endpoint = f"/api/v4/projects/{encoded_project_id}/repository/tags?search=^{prefix}&order-by=version"

    connection.request("GET", endpoint)
    response = connection.getresponse()

    if response.status == 200:
        tags_data = response.read().decode("utf-8")
        tags = json.loads(tags_data)
        return tags
    else:
        print(f"Error: {response.status} - {response.reason}")
        return None

project_id = "ghc/ghc"

# matches GHC release versions
version_re = re.compile("ghc-(?P<version>.*)-release")

def parse_version(s):
    return tuple(map(int, s.split(".")))

def main():
    SCRIPT_PATH = Path(__file__)

    if len(sys.argv) != 2:
        sys.exit(f"usage: {SCRIPT_PATH.name} GHC_MAJOR_MINOR")

    bindist = import_path(str(SCRIPT_PATH.parent.parent.joinpath('haskell/private/ghc_bindist_generated.bzl')))

    GHC_MAJOR_MINOR = sys.argv[1]

    versions = [ version for version in bindist.GHC_BINDIST if version.startswith(GHC_MAJOR_MINOR) ]

    prefix = f"ghc-{GHC_MAJOR_MINOR}."

    tags = get_gitlab_tags_with_prefix(project_id, prefix)

    if tags:
        releases = [ m.group("version") for tag in tags if (m := version_re.match(tag["name"])) ]

        latest_release = releases[0]

        if latest_release in versions:
            print(f"GHC {GHC_MAJOR_MINOR} is up-to-date", file=sys.stderr)
        else:
            print("found update:", latest_release, file=sys.stderr)

            replace = re.compile(r'^(?P<indent>\s+)\{\s*"version"\s*:\s*"' + re.escape(GHC_MAJOR_MINOR + '.'), re.MULTILINE)

            gen_script_path = SCRIPT_PATH.parent.parent.joinpath('haskell/gen_ghc_bindist.py')
            with open(gen_script_path, 'r+') as gen:
                gen_script = gen.read()

                print(" 1. modify haskell/gen_ghc_bindist.py", file=sys.stderr)

                added_version = replace.sub(fr'''\g<indent>{{ "version": { repr(latest_release) },
\g<indent>  "ignore_suffixes": [".bz2", ".lz", ".zip"] }},
\g<0>''', gen_script, count=1)

                if added_version is gen_script:
                    sys.exit(f"could not add new version {latest_release} using regex {replace}")
                else:
                    gen.truncate(0)
                    gen.seek(0)
                    gen.write(added_version)

            print(" 2. call haskell/gen_ghc_bindist.py", file=sys.stderr)

            bzl = check_output([sys.executable, 'haskell/gen_ghc_bindist.py'])

            print(" 3. format bzl code, write to haskell/private/ghc_bindist_generated.bzl", file=sys.stderr)
            with open('haskell/private/ghc_bindist_generated.bzl', 'r+') as generated:
                run(['buildifier'], check=True, input=bzl, stdout=generated)
                generated.truncate()
    else:
        print("no tags found for prefix", prefix, file=sys.stderr)


if __name__ == '__main__':
    main()
