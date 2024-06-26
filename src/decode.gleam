// TODO: one_of

import gleam/dict.{type Dict}
import gleam/dynamic.{type DecodeError, type Dynamic, DecodeError}
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result

/// The result that a decoder runs when run.
pub type DecodeResult(t) =
  Result(t, List(dynamic.DecodeError))

/// A decoder is a value that can be used to turn dynamically typed `Dynamic`
/// data into typed data using the `from` function.
///
/// Several smaller decoders can be combined to make larger decoders using
/// functions such as `list` and `field`.
///
pub opaque type Decoder(t) {
  Decoder(continuation: fn(Dynamic) -> DecodeResult(t))
}

/// Create a new decoder for a given constructor function. If this function is
/// a function that takes parameters one-at-a-time, such as anonymous functions
/// made with `use name <- decode.parameter` then the decoder can be used with
/// the `decode.field` function to decode a value that contains multiple other
/// values.
///
/// # Examples
///
/// ```gleam
/// let data = dynamic.from(dict.from_list([
///   #("email", "lucy@example.com"),
///   #("name", "Lucy"),
/// ]))
///
/// decode.into({
///   use name <- decode.parameter
///   use email <- decode.parameter
///   SignUp(name: name, email: email)
/// })
/// |> decode.field("name", string)
/// |> decode.field("email", string)
/// |> decode.from(data)
/// Ok(SignUp(name: "Lucy", email: "lucy@example.com"))
/// ```
///
pub fn into(constructor: t1) -> Decoder(t1) {
  Decoder(continuation: fn(_) { Ok(constructor) })
}

/// This function is used to create constructor functions that take arguments
/// one at a time, making them suitable for passing to the `into` function.
///
/// # Examples
///
/// ```gleam
/// let data = dynamic.from(dict.from_list([
///   #("email", "lucy@example.com"),
///   #("name", "Lucy"),
/// ]))
///
/// decode.into({
///   use name <- decode.parameter
///   use email <- decode.parameter
///   SignUp(name: name, email: email)
/// })
/// |> decode.field("name", string)
/// |> decode.field("email", string)
/// |> decode.from(data)
/// Ok(SignUp(name: "Lucy", email: "lucy@example.com"))
/// ```
///
pub fn parameter(body: fn(t1) -> t2) -> fn(t1) -> t2 {
  body
}

/// Run a decoder on a `Dynamic` value, decoding the value if it is of the
/// desired type, or returning errors.
///
/// The second parameter is a field name which will be used to index into the
/// `Dynamic` data.
/// This function will index into dictionaries with any key type, and if the key is
/// an int then it'll also index into Erlang tuples and JavaScript arrays.
///
/// # Examples
///
/// ```gleam
/// let data = dynamic.from(dict.from_list([
///   #("email", "lucy@example.com"),
///   #("name", "Lucy"),
/// ]))
///
/// decode.into({
///   use name <- decode.parameter
///   use email <- decode.parameter
///   SignUp(name: name, email: email)
/// })
/// |> decode.field("name", string)
/// |> decode.field("email", string)
/// |> decode.from(data)
/// Ok(SignUp(name: "Lucy", email: "lucy@example.com"))
/// ```
///
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

/// Run a decoder on a `Dynamic` value, decoding the value if it is of the
/// desired type, or returning errors.
///
/// # Examples
///
/// ```gleam
/// decode.into({
///   use name <- decode.parameter
///   use email <- decode.parameter
///   SignUp(name: name, email: email)
/// })
/// |> decode.field("email", string)
/// |> decode.field("password", string)
/// |> decode.from(data)
/// ```
///
pub fn from(decoder: Decoder(t), data: Dynamic) -> DecodeResult(t) {
  decoder.continuation(data)
}

/// A decoder that decodes `String` values.
///
/// # Examples
///
/// ```gleam
/// decoder.string
/// |> decoder.from(dynamic.from("Hello!"))
/// // -> Ok("Hello!")
/// ```
///
pub const string: Decoder(String) = Decoder(continuation: dynamic.string)

/// A decoder that decodes `Bool` values.
///
/// # Examples
///
/// ```gleam
/// decoder.bool
/// |> decoder.from(dynamic.from(True))
/// // -> Ok(True)
/// ```
///
pub const bool: Decoder(Bool) = Decoder(continuation: dynamic.bool)

/// A decoder that decodes `Int` values.
///
/// # Examples
///
/// ```gleam
/// decoder.int
/// |> decoder.from(dynamic.from(147))
/// // -> Ok(147)
/// ```
///
pub const int: Decoder(Int) = Decoder(continuation: dynamic.int)

/// A decoder that decodes `Float` values.
///
/// # Examples
///
/// ```gleam
/// decoder.float
/// |> decoder.from(dynamic.from(3.14))
/// // -> Ok(3.14)
/// ```
///
pub const float: Decoder(Float) = Decoder(continuation: dynamic.float)

/// A decoder that decodes `Dynamic` values. This decoder never returns an error.
///
/// # Examples
///
/// ```gleam
/// decoder.dynamic
/// |> decoder.from(dynamic.from(3.14))
/// // -> Ok(dynamic.from(3.13))
/// ```
///
pub const dynamic: Decoder(Dynamic) = Decoder(continuation: Ok)

/// A decoder that decodes `BitArray` values. This decoder never returns an error.
///
/// # Examples
///
/// ```gleam
/// decoder.bit_array
/// |> decoder.from(dynamic.from(<<5, 7>>))
/// // -> Ok(<<5, 7>>)
/// ```
///
pub const bit_array: Decoder(BitArray) = Decoder(
  continuation: dynamic.bit_array,
)

/// A decoder that decodes lists where all elements are decoded with a given
/// decoder.
///
/// # Examples
///
/// ```gleam
/// decoder.list(of: decode.int)
/// |> decoder.from(dynamic.from([1, 2, 3]))
/// // -> Ok([1, 2, 3])
/// ```
///
pub fn list(of item: Decoder(a)) -> Decoder(List(a)) {
  Decoder(continuation: dynamic.list(item.continuation))
}

/// A decoder that decodes dicts where all keys and vales are decoded with
/// given decoders.
///
/// # Examples
///
/// ```gleam
/// let values = dict.from_list([
///   #("one", 1),
///   #("two", 2),
/// ])
/// decoder.dict(decode.string, decode.int)
/// |> decoder.from(dynamic.from(values))
/// // -> Ok(values)
/// ```
///
pub fn dict(
  key: Decoder(key),
  value: Decoder(value),
) -> Decoder(Dict(key, value)) {
  Decoder(continuation: dynamic.dict(key.continuation, value.continuation))
}

/// A decoder that decodes nullable values of a type decoded by with a given
/// decoder.
///
/// This function common representations of null on all runtimes, such as
/// `nil`, `null`, and `undefined` on Erlang, and `undefined` and `null` on
/// JavaScript.
///
/// # Examples
///
/// ```gleam
/// decoder.optional(of: decode.int)
/// |> decoder.from(dynamic.from(100))
/// // -> Ok(option.Some(100))
/// ```
///
/// ```gleam
/// decoder.optional(of: decode.int)
/// |> decoder.from(dynamic.from(Nil))
/// // -> Ok(option.None)
/// ```
///
pub fn optional(item: Decoder(a)) -> Decoder(Option(a)) {
  Decoder(continuation: dynamic.optional(item.continuation))
}

/// A decoder that decodes a value that is nested within other values. For
/// example, decoding a value that is within some deeply nested JSON objects.
///
/// This function will index into dictionaries with any key type, and if the key is
/// an int then it'll also index into Erlang tuples and JavaScript arrays.
///
/// # Examples
///
/// ```gleam
/// let data = dynamic.from(dict.from_list([
///   #("one", dict.from_list([
///     #("two", 1000),
///   ])),
/// ]))
///
/// decoder.at(["one", "two"], decode.int)
/// |> decoder.from(data)
/// // -> Ok(1000)
/// ```
///
/// ```gleam
/// decoder.optional(of: decode.int)
/// |> decoder.from(dynamic.from(Nil))
/// // -> Ok(option.None)
/// ```
///
pub fn at(path: List(segment), inner: Decoder(a)) -> Decoder(a) {
  Decoder(continuation: fn(data) {
    let decoder =
      list.fold_right(path, inner.continuation, fn(dyn_decoder, segment) {
        index(segment, dyn_decoder, _)
      })
    decoder(data)
  })
}

// Indexes into either a tuple/array, or a dict/map/object depending on the key
fn index(key: a, inner: dynamic.Decoder(b), data: Dynamic) -> DecodeResult(b) {
  case bare_index(data, key) {
    Ok(data) ->
      case inner(data) {
        Ok(data) -> Ok(data)
        Error(errors) -> Error(push_path(errors, key))
      }
    Error(kind) -> Error([DecodeError(kind, dynamic.classify(data), [])])
  }
}

@external(erlang, "decode_ffi", "index")
@external(javascript, "./decode_ffi.mjs", "index")
fn bare_index(data: Dynamic, key: anything) -> Result(Dynamic, String)

/// Apply a transformation function to any value decoded by the decoder.
///
/// # Examples
///
/// ```gleam
/// decoder.int
/// |> decoder.map(int.to_string)
/// |> decoder.from(dynamic.from(1000))
/// // -> Ok("1000")
/// ```
///
pub fn map(decoder: Decoder(a), transformer: fn(a) -> b) -> Decoder(b) {
  Decoder(continuation: fn(d) {
    case decoder.continuation(d) {
      Ok(a) -> Ok(transformer(a))
      Error(e) -> Error(e)
    }
  })
}

/// Apply a transformation function to any errors returned by the decoder.
///
pub fn map_errors(
  decoder: Decoder(a),
  transformer: fn(List(DecodeError)) -> List(DecodeError),
) -> Decoder(a) {
  Decoder(continuation: fn(d) {
    case decoder.continuation(d) {
      Ok(a) -> Ok(a)
      Error(e) -> Error(transformer(e))
    }
  })
}

/// Replace all errors produced by a decoder with one single error for a named
/// expected type.
///
/// # Examples
///
/// ```gleam
/// decoder.string
/// |> decoder.collapse_errors("MyThing")
/// |> decoder.from(dynamic.from(1000))
/// // -> Error([DecodeError("MyThing", "Int", [])])
/// ```
///
pub fn collapse_errors(decoder: Decoder(a), name: String) -> Decoder(a) {
  Decoder(continuation: fn(d) {
    case decoder.continuation(d) {
      Ok(a) -> Ok(a)
      Error(e) -> Error([DecodeError(name, dynamic.classify(d), [])])
    }
  })
}

/// Create a new decoder based upon the value of a previous decoder.
///
/// This may be useful for when you need to know some of the structure of the
/// dynamic value in order to know how to decode the rest of it.
///
pub fn then(decoder: Decoder(a), next: fn(a) -> Decoder(b)) -> Decoder(b) {
  Decoder(continuation: fn(d) {
    case decoder.continuation(d) {
      Ok(a) -> next(a) |> from(d)
      Error(e) -> Error(e)
    }
  })
}

fn push_path(errors: List(DecodeError), key: t) -> List(DecodeError) {
  let key = dynamic.from(key)
  let decoder =
    dynamic.any([
      dynamic.string,
      fn(x) { result.map(dynamic.int(x), int.to_string) },
    ])
  let key = case decoder(key) {
    Ok(key) -> key
    Error(_) -> "<" <> dynamic.classify(key) <> ">"
  }
  list.map(errors, fn(error) { DecodeError(..error, path: [key, ..error.path]) })
}
