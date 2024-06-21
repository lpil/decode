// TODO: at
// TODO: optional
// TODO: optional_field
// TODO: element? Something for tuples.
// TODO: a function for making a decoder from a result returning function
// TODO: combinators: map
// TODO: combinators: monadic bind (what should it be called? Try?)

import gleam/dynamic.{type Dynamic}
import gleam/list

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

// TODO: test
// TODO: document
pub fn field(
  decoder: Decoder(fn(t1) -> t2),
  field_name: String,
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
pub fn list(item: Decoder(a)) {
  Decoder(continuation: dynamic.list(item.continuation))
}

// TODO: document
pub const shallow_list: Decoder(List(Dynamic)) = Decoder(
  continuation: dynamic.shallow_list,
)
