// TODO: one_of
// TODO: optional_field
// TODO: a function for making a decoder from a result returning function
// TODO: combinators: map
// TODO: combinators: monadic bind (what should it be called? Try?)

import gleam/dynamic.{type Dynamic, DecodeError}
import gleam/list
import gleam/option.{type Option}

// TODO: document
pub type DecodeResult(t) =
  Result(t, List(dynamic.DecodeError))

// TODO: document
pub opaque type Decoder(t) {
  Decoder(continuation: fn(Dynamic) -> DecodeResult(t))
}

// TODO: document
pub fn into(constructor: t1) -> Decoder(t1) {
  Decoder(continuation: fn(_) { Ok(constructor) })
}

// TODO: document
pub fn parameter(body: fn(t1) -> t2) -> fn(t1) -> t2 {
  body
}

// TODO: document
pub fn field(
  decoder: Decoder(fn(t1) -> t2),
  field_name: name,
  field_decoder: Decoder(t1),
) -> Decoder(t2) {
  Decoder(continuation: fn(data) {
    let constructor = decoder.continuation(data)
    let data = dynamic.field(field_name, from(field_decoder, _))(data)
    case constructor, data {
      Ok(constructor), Ok(data) -> Ok(constructor(data))
      Error(e1), Error(e2) -> Error(list.append(e1, e2))
      _, Error(errors) | Error(errors), _ -> Error(errors)
    }
  })
}

// TODO: document
pub fn from(decoder: Decoder(t), data: Dynamic) -> DecodeResult(t) {
  decoder.continuation(data)
}

// TODO: document
pub const string: Decoder(String) = Decoder(continuation: dynamic.string)

// TODO: document
pub const bool: Decoder(Bool) = Decoder(continuation: dynamic.bool)

// TODO: document
pub const int: Decoder(Int) = Decoder(continuation: dynamic.int)

// TODO: document
pub const float: Decoder(Float) = Decoder(continuation: dynamic.float)

// TODO: document
pub const bit_array: Decoder(BitArray) = Decoder(
  continuation: dynamic.bit_array,
)

// TODO: document
pub fn list(item: Decoder(a)) -> Decoder(List(a)) {
  Decoder(continuation: dynamic.list(item.continuation))
}

// TODO: document
pub fn optional(item: Decoder(a)) -> Decoder(Option(a)) {
  Decoder(continuation: dynamic.optional(item.continuation))
}

// TODO: document
pub const shallow_list: Decoder(List(Dynamic)) = Decoder(
  continuation: dynamic.shallow_list,
)

// TODO: document
// TODO: document that it works with tuples too
pub fn at(path: List(segment), inner: Decoder(a)) -> Decoder(a) {
  Decoder(continuation: fn(data) {
    let decoder =
      list.fold_right(path, inner.continuation, fn(dyn_decoder, segment) {
        flexible_index(segment, dyn_decoder, _)
      })
    decoder(data)
  })
}

// Indexes into either a tuple/array, or a dict/map/object depending on the key
fn flexible_index(
  key: a,
  inner: dynamic.Decoder(b),
  data: Dynamic,
) -> DecodeResult(b) {
  case shallow_index(data, key) {
    Ok(data) -> inner(data)
    Error(_) ->
      case dynamic.field(key, inner)(data) {
        Ok(data) -> Ok(data)
        Error([DecodeError(_, found, [])]) ->
          Error([DecodeError("Indexable", found, [])])
        Error(errors) -> Error(errors)
      }
  }
}

@external(erlang, "decode_ffi", "index")
@external(javascript, "./decode_ffi.mjs", "index")
fn shallow_index(data: Dynamic, key: anything) -> DecodeResult(Dynamic)
