load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_doc",
    "haskell_library",
    "haskell_proto_library",
)

package(default_testonly = 1)

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

proto_library(
    name = "stripped_zip_code_proto",
    srcs = ["stripped_zip_code.proto"],
    strip_import_prefix = "/tests/haskell_proto_library/",
)

proto_library(
    name = "stripped_address_proto",
    srcs = ["stripped_address.proto"],
    strip_import_prefix = "/tests/haskell_proto_library/",
    deps = [":stripped_zip_code_proto"],
)

haskell_proto_library(
    name = "address_haskell_proto",
    tags = ["requires_hackage"],
    deps = [":address_proto"],
)

haskell_proto_library(
    name = "stripped_address_haskell_proto",
    tags = ["requires_hackage"],
    deps = [":stripped_address_proto"],
)

haskell_proto_library(
    name = "person_haskell_proto",
    tags = ["requires_hackage"],
    deps = [":person_proto"],
)

haskell_library(
    name = "hs-lib",
    srcs = ["Bar.hs"],
    tags = ["requires_hackage"],
    visibility = ["//visibility:public"],
    deps = [
        ":address_haskell_proto",
        ":person_haskell_proto",
        ":stripped_address_haskell_proto",
        "//tests/hackage:base",
    ],
)

haskell_doc(
    name = "docs",
    tags = ["requires_hackage"],
    deps = [":hs-lib"],
)
