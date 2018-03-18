load("@bazel_skylib//:lib.bzl", "asserts", "unittest")
load("//lib:json_parser.bzl", "json_parse")


def _valid_json_parse_test(ctx):
    env = unittest.begin(ctx)

    asserts.equals(env, json_parse('[]'), [])
    asserts.equals(
        env,
        json_parse('["x", "y", 22, [7], {"z": 1, "y": null}]'),
        ["x", "y", 22, [7], {"z" : 1, "y" : None}])
    asserts.equals(
        env,
        " ".join(reversed(json_parse('["plain", "the", "on", "mainly", "falls", "spain", "in", "rain", "the"]'))),
        "the rain in spain falls mainly on the plain",
    )
    asserts.equals(
        env,
        json_parse('["a", "b", "c", [1, 2, 3, [4], [5], [6]]]'),
        ["a", "b", "c", [1, 2, 3, [4], [5], [6]]])
    asserts.equals(env, json_parse('{}'), {})
    asserts.equals(env, json_parse('{"a" : "b"}'), { "a" : "b" })
    asserts.equals(
        env,
        json_parse('{"key1": [1, 2, ["nested"]], "key2": "val2", "key3": {"nested_key1" : [null, true, false]}}'),
        {
            "key1" : [1, 2, ["nested"]],
            "key2" : "val2",
            "key3" : {
                "nested_key1" : [None, True, False]
            }
        })

    unittest.end(env)


def _json_scalar_types_test(ctx):
    env = unittest.begin(ctx)

    asserts.equals(env, json_parse('[""]')[0], "")
    asserts.equals(env, json_parse('["a string"]')[0], "a string")
    asserts.equals(env, json_parse('[true]')[0], True)
    asserts.equals(env, json_parse('[false]')[0], False)
    asserts.equals(env, json_parse('[null]')[0], None)
    asserts.equals(env, json_parse('[100]')[0], 100)
    asserts.equals(env, json_parse('[-100]')[0], -100)

    print("TODO: find a solution for non-integer reductions, see TODO in json_parser.bzl")
    asserts.equals(env, json_parse('[1e100]')[0], 1)
    asserts.equals(env, json_parse('[1.11]')[0], 1)

    unittest.end(env)


def _max_depth_json_parse_test(ctx):
    env = unittest.begin(ctx)

    asserts.equals(
        env,
        json_parse('[[[[[[[[[[[[[[[[[[[[20]]]]]]]]]]]]]]]]]]]]'),
        [[[[[[[[[[[[[[[[[[[[20]]]]]]]]]]]]]]]]]]]]
    )

    asserts.equals(
        env,
        json_parse('[[["too_deep"]]]', fail_on_invalid = False, max_depth = 2),
        None
    )

    unittest.end(env)

valid_json_parse_test = unittest.make(_valid_json_parse_test)
json_scalar_types_test = unittest.make(_json_scalar_types_test)
max_depth_json_parse_test = unittest.make(_max_depth_json_parse_test)

def json_parse_test_suite():
    """Creates the test targets and test suite for //lib:json_parse.bzl."""
    unittest.suite(
        "json_parse_tests",
        valid_json_parse_test,
        json_scalar_types_test,
        max_depth_json_parse_test,
    )
