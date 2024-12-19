# Changelog

## v1.1.0 - 2024-12-19

- Added the `zero.recursive` function.

## v1.0.0 - 2024-12-10

- `zero.dict` can now decode JavaScript maps, weakmaps, and objects without a
  prototype.

## v0.5.0 - 2024-11-15

- The `field`, `subfield`, and `at` functions in `decode/zero` no longer return
  a default value when a field is not present, instead an error is returned.
- `decode/zero` module gains the `optional_field` and `optionally_at` functions.
- Fixed a bug in `decode/zero` with indexing with `0` on JavaScript.

## v0.4.1 - 2024-11-01

- Fixed spelling of `new_primitive_decoder`.

## v0.4.0 - 2024-10-31

- The `decode/zero` module gains `new_primitive_decoder` function.

## v0.3.0 - 2024-09-18

- Added `decode/zero`, a module that provides a new decoder API.

## v0.2.1 - 2024-09-01

- Fixed JavaScript support.

## v0.2.0 - 2024-06-30

- Added the `subfield` function.
- Fixed a bug where `field` would not index into tuples.

## v0.1.0 - 2024-06-29

- Initial release.
