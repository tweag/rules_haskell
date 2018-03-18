load("//lib:json_rules.bzl",
     "json_type_null",
     "json_type_boolean",
     "json_type_string",
     "json_type_number",
     "json_type_array",
     "json_type_object",
     "json_printer",
     "json",
)

load("//lib:json_checker.bzl",
     "json_parse")

print(json_parse('{"key1": [1, 2, ["nested"]], "key2": "val2", "key3": {"nested_key1" : [null, true, false]}}'))
print(json_parse('["x", "y", 22, [7], {"z": 1, "y": null}]'))
print(json_parse('{"x": "a", "y" : 9}'))
print(json_parse('{"foo": "bar", "biz": [1,20,337, true, false, null, 1e17, 0.17, {"a": "b"}]}'))
print(json_parse('[[[[[[[[[[10]]]]]]]]]]'))

# Default values
json_type_null(name = "null_check")
json_type_boolean(name = "boolean_check")
json_type_string(name = "string_check")
json_type_number(name = "number_check")
json_type_array(name = "array_check")
json_type_object(name = "object_check")

# Value checks
json_type_boolean(
    name = "boolean_check_false",
    boolean_value = False,
)
json_type_boolean(
    name = "boolean_check_true",
    boolean_value = True,
)

json_type_string(
    name = "string_check_non_empty",
    string_value = "my_json_string",
)

json_type_string(
    name = "string_check_quoted",
    string_value = "my 'quoted' json_string",
)

json_type_number(
    name = "number_check_77",
    number_value = 77,
)

json_type_number(
    name = "number_check_fraction",
    number_value = 77,
    exponent = -2,
)

json_type_array(
    name = "array_check_empty",
    array_values = [],
)

json_type_array(
    name = "array_check_values",
    array_values = [
        ":null_check",
        ":boolean_check",
        ":string_check",
        ":number_check",
        ":array_check",
        ":object_check",
    ],
)

json_type_object(
    name = "object_check_empty",
    inverted_object_map = {
        ":array_check_values" : "an_array",
    }
)

json_printer(
    name = "my_printer",
    json_values = [
        ":null_check",
        ":boolean_check",
        ":string_check",
        ":string_check_quoted",
        ":string_check_non_empty",
        ":number_check_77",
        ":number_check_fraction",
        ":array_check_values",
        ":object_check_empty",
    ],
)

json(None)

json("a string")

json(66)

json(["a", "b"])

json({
    "foo" : "bar"
})
