# Copyright 2018 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# converted from extension //third_party/haskell/alex:build_defs
# Genrule for the Alex scanner generator.
#
# Example:
#   load("//third_party/haskell/alex:build_defs.bzl", "genalex")
#
#   genalex(
#       src = "MyScanner.y",
#       out = "MyScanner.y.hs"
#   )
#
#   haskell_binary(
#       name = "MyScanner",
#       srcs = [ "MyScanner.y.hs" ]
#       ...
#   )
load("//tools:mangling.bzl", "hazel_binary")
def genalex(src, out):
  native.genrule(
      name=out + ".hs_alex",
      srcs=[src],
      outs=[out],
      tools=[hazel_binary("alex")],
      cmd="$(location {}) -g -o $(OUTS) $(SRCS)".format(hazel_binary("alex")),
  )
