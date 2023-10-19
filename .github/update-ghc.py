#!/usr/bin/env python

import os, sys, re
import http.client
import json
from pathlib import PurePosixPath, Path
from pprint import pprint
from urllib.parse import quote
from subprocess import check_call, run

GHC_GITLAB_SERVER = "gitlab.haskell.org"


def get_gitlab_tags_with_prefix(project_id, prefix):
    connection = http.client.HTTPSConnection(GHC_GITLAB_SERVER)
    encoded_project_id = encoded_project_id = quote(project_id, safe="")
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

    GHC_MAJOR_MINOR = sys.argv[1]

    prefix = f"ghc-{GHC_MAJOR_MINOR}."

    tags = get_gitlab_tags_with_prefix(project_id, prefix)

    if not tags:
        print("no tags found for prefix", prefix, file=sys.stderr)
        sys.exit(0)

    with open(
        SCRIPT_PATH.parent.parent.joinpath(
            "haskell/private/ghc_bindist_generated.json"
        ),
        "r+",
    ) as bindist_json:
        bindists = json.load(bindist_json)

        versions = [
            version for version in bindists if version.startswith(GHC_MAJOR_MINOR)
        ]

        releases = [
            m.group("version") for tag in tags if (m := version_re.match(tag["name"]))
        ]

        latest_release = releases[0]

        if latest_release in versions:
            print(f"GHC {GHC_MAJOR_MINOR} is up-to-date", file=sys.stderr)
            sys.exit(0)

        print("found update:", latest_release, file=sys.stderr)

        print(" 1. modify haskell/private/ghc_bindist_generated.json", file=sys.stderr)

        bindists[latest_release] = {}

        bindist_json.truncate(0)
        bindist_json.seek(0)
        json.dump(bindists, bindist_json)

    gen_script_path = SCRIPT_PATH.parent.parent.joinpath("haskell/gen_ghc_bindist.py")

    print(" 2. call haskell/gen_ghc_bindist.py", file=sys.stderr)

    check_call([sys.executable, "haskell/gen_ghc_bindist.py"])

    if "GITHUB_OUTPUT" in os.environ:
        with open(os.environ["GITHUB_OUTPUT"], "a") as output:
            print(f"latest={ latest_release }", file=output)


if __name__ == "__main__":
    main()
