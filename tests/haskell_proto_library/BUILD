package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_doc",
    "haskell_library",
    "haskell_proto_library",
)

proto_library(
    name = "zip_code_proto",
    srcs = ["zip_code.proto"],
)

proto_library(
    name = "address_proto",
    srcs = ["address.proto"],
    deps = [":zip_code_proto"],
)

proto_library(
    name = "person_proto",
    srcs = ["person.proto"],
    deps = [
        ":address_proto",
        "@com_google_protobuf//:timestamp_proto",
    ],
)

haskell_proto_library(
    name = "address_haskell_proto",
    deps = [":address_proto"],
)

haskell_proto_library(
    name = "person_haskell_proto",
    deps = [":person_proto"],
)

haskell_library(
    name = "hs-lib",
    srcs = ["Bar.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":address_haskell_proto",
        ":person_haskell_proto",
        "//tests:base",
    ],
)

haskell_doc(
    name = "docs",
    deps = [":hs-lib"],
)
