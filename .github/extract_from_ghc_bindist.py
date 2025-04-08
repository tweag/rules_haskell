import json
import os
import sys
from packaging import version


def get_latest_versions(version_list):
    # Create a dictionary to store the latest version for each major.minor pair
    latest_versions = {}

    for ver in version_list:
        # Split the version into major, minor, and patch
        major, minor, patch = map(int, ver.split('.'))

        # Create a key for the major.minor pair
        key = f"{major}.{minor}"

        # If the key is not in the dictionary or the current version is greater than the stored version
        if key not in latest_versions or version.parse(ver) > version.parse(latest_versions[key]):
            # Update the dictionary with the latest version
            latest_versions[key] = ver

    # Return the latest versions
    return list(latest_versions.values())


current_dir = os.path.dirname(__file__)
haskell_dir = os.path.join(current_dir, "..", "haskell")

with open(
    os.path.join(haskell_dir, "private", "ghc_bindist_generated.json"), mode="rb"
) as f:
    version_numbers = get_latest_versions(json.load(f).keys())

with open(os.environ["GITHUB_OUTPUT"], mode="a", encoding="utf-8") as output:
    output.write("ghc-matrix={}\n".format(version_numbers))
