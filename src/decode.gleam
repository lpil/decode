import gleam/dynamic.{type Dynamic}
import gleam/list

pub type DecodeResult(t) =
  Result(t, List(dynamic.DecodeError))

pub opaque type Decoder(t) {
  Decoder(continuation: fn(Dynamic) -> DecodeResult(t))
}

pub fn into(constructor: t1) -> Decoder(t1) {
  Decoder(continuation: fn(_) { Ok(constructor) })
}

pub fn parameter(body: fn(t1) -> t2) -> fn(t1) -> t2 {
  body
}

pub fn field(
  decoder: Decoder(fn(t1) -> t2),
  field_name: String,
  field_decoder: Decoder(t1),
) -> Decoder(t2) {
  Decoder(continuation: fn(data) {
    let constructor = decoder.continuation(data)
    let data = dynamic.field(field_name, run(field_decoder, _))(data)
    case constructor, data {
      Ok(constructor), Ok(data) -> Ok(constructor(data))
      Error(e1), Error(e2) -> Error(list.append(e1, e2))
      _, Error(errors) | Error(errors), _ -> Error(errors)
    }
  })
}

pub fn run(decoder: Decoder(t), data: Dynamic) -> DecodeResult(t) {
  decoder.continuation(data)
}

pub const string: Decoder(String) = Decoder(continuation: dynamic.string)

pub const bool: Decoder(Bool) = Decoder(continuation: dynamic.bool)

pub const int: Decoder(Int) = Decoder(continuation: dynamic.int)

pub const float: Decoder(Float) = Decoder(continuation: dynamic.float)
