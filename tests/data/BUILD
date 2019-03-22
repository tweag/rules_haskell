# Generic data files and targets that are used by multiple tests

cc_library(
    name = "ourclibrary",
    srcs = [":ourclibrary.c"],
    linkstatic = False,
    visibility = ["//visibility:public"],
)

cc_library(
    name = "ourclibrary-static",
    srcs = [":ourclibrary.c"],
    linkstatic = True,
    visibility = ["//visibility:public"],
)
