load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
)
load("@ai_formation_hazel//:hazel.bzl", "hazel_library")

haskell_library(
    name = "effect",
    srcs = glob(["src/**/*.hs"]),
    src_strip_prefix = "src",
    visibility = ["//visibility:public"],
    deps = [
        hazel_library("errors"),
        hazel_library("lens"),
        hazel_library("mtl"),
        hazel_library("murmur-hash"),
        hazel_library("primitive"),
        hazel_library("profunctors"),
        hazel_library("random-fu"),
        hazel_library("random-source"),
        hazel_library("resourcet"),
        hazel_library("rvar"),
        hazel_library("tagged"),
        hazel_library("tf-random"),
        hazel_library("transformers-lift"),
        hazel_library("writer-cps-transformers"),
        hazel_library("base"),
        hazel_library("time"),
        hazel_library("transformers"),
    ],
)

load("//bzl:hspec_discover.bzl", "hspec_discover_test")

hspec_discover_test(
    name = "hspec",
    size = "small",
    srcdir = "test",
    deps = [
        ":effect",
        hazel_library("QuickCheck"),
        hazel_library("base"),
        hazel_library("hspec"),
        hazel_library("mersenne-random-pure64"),
    ],
)

haskell_binary(
    name = "bench",
    testonly = 1,
    srcs = ["bench/Bench.hs"],
    src_strip_prefix = "bench",
    deps = [
        "//lib/effect",
        hazel_library("base"),
        hazel_library("criterion"),
        hazel_library("mersenne-random-pure64"),
        hazel_library("mwc-random"),
        hazel_library("random"),
        hazel_library("random-fu"),
        hazel_library("tf-random"),
        hazel_library("vector"),
    ],
)
