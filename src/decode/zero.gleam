//// The `Dynamic` type is used to represent dynamically typed data. That is, data
//// that we don't know the precise type of yet, so we need to introspect the data to
//// see if it is of the desired type before we can use it. Typically data like this
//// would come from user input or from untyped languages such as Erlang or JavaScript.
////
//// This module provides the `Decoder` type and associated functions, which provides
//// a type-safe and composable way to convert dynamic data into some desired type,
//// or into errors if the data doesn't have the desired structure.
////
//// The `Decoder` type is generic and has 1 type parameter, which is the type that
//// it attempts to decode. A `Decoder(String)` can be used to decode strings, and a
//// `Decoder(Option(Int))` can be used to decode `Option(Int)`s
////
//// Decoders work using _runtime reflection_ and the data structures of the target
//// platform. Differences between Erlang and JavaScript data structures may impact
//// your decoders, so it is important to test your decoders on all supported
//// platforms.
////
//// # Examples
////
//// Dynamic data may come from various sources and so many different syntaxes could
//// be used to describe or construct them. In these examples a pseudocode
//// syntax is used to describe the data.
////
//// ## Simple types
////
//// This module defines decoders for simple data types such as [`string`](#string),
//// [`int`](#int), [`float`](#float), [`bit_array`](#bit_array), and [`bool`](#bool).
////
//// ```gleam
//// // Data:
//// // "Hello, Joe!"
////
//// let result =
////   decode.string
////   |> decode.run(data)
////
//// assert result == Ok("Hello, Joe!")
//// ```
////
//// ## Lists
////
//// The [`list`](#list) decoder decodes `List`s. To use it you must construct it by
//// passing in another decoder into the `list` function, which is the decoder that
//// is to be used for the elements of the list, type checking both the list and its
//// elements.
////
//// ```gleam
//// // Data:
//// // [1, 2, 3, 4]
////
//// let result =
////   decode.list(decode.int)
////   |> decode.run(data)
////
//// assert result == Ok([1, 2, 3, 4])
//// ```
////
//// On Erlang this decoder can decode from lists, and on JavaScript it can
//// decode from lists as well as JavaScript arrays.
////
//// ## Options
////
//// The [`optional`](#optional) decoder is used to decode values that may or may not
//// be present. In other environment these might be called "nullable" values.
////
//// Like the `list` decoder, the `optional` decoder takes another decoder,
//// which is used to decode the value if it is present.
////
//// ```gleam
//// // Data:
//// // 12.45
////
//// let result =
////   decode.optional(decode.int)
////   |> decode.run(data)
////
//// assert result == Ok(option.Some(12.45))
//// ```
//// ```gleam
//// // Data:
//// // null
////
//// let result =
////   decode.optional(decode.int)
////   |> decode.run(data)
////
//// assert result == Ok(option.None)
//// ```
////
//// This decoder knows how to handle multiple different runtime representations of
//// absent values, including `Nil`, `None`, `null`, and `undefined`.
////
//// ## Dicts
////
//// The [`dict`](#dict) decoder decodes `Dicts` and contains two other decoders, one
//// for the keys, one for the values.
////
//// ```gleam
//// // Data:
//// // { "Lucy" -> 10, "Nubi" -> 20 }
////
//// let result =
////   decode.dict(decode.string, decode.int)
////   |> decode.run(data)
////
//// assert result == Ok(dict.from_list([
////   #("Lucy", 10),
////   #("Nubi", 20),
//// ]))
//// ```
////
//// ## Indexing objects
////
//// The [`at`](#at) decoder can be used to decode a value that is nested within
//// key-value containers such as Gleam dicts, Erlang maps, or JavaScript objects.
////
//// ```gleam
//// // Data:
//// // { "one" -> { "two" -> 123 } }
////
//// let result =
////   decode.at(["one", "two"], decode.int)
////   |> decode.run(data)
////
//// assert result == Ok(123)
//// ```
////
//// ## Indexing arrays
////
//// If you use ints as keys then the [`at`](#at) decoder can be used to index into
//// array-like containers such as Gleam or Erlang tuples, or JavaScript arrays.
////
//// ```gleam
//// // Data:
//// // ["one", "two", "three"]
////
//// let result =
////   decode.at([1], decode.string)
////   |> decode.run(data)
////
//// assert result == Ok("two")
//// ```
////
//// ## Records
////
//// Decoding records from dynamic data is more complex and requires combining a
//// decoder for each field and a special constructor that builds your records with
//// the decoded field values.
////
//// ```gleam
//// // Data:
//// // {
//// //   "score" -> 180,
//// //   "name" -> "Mel Smith",
//// //   "is-admin" -> false,
//// //   "enrolled" -> true,
//// //   "colour" -> "Red",
//// // }
////
//// let decoder = {
////   use name <- decode.field("name", decode.string)
////   use score <- decode.field("score", decode.int)
////   use colour <- decode.field("colour", decode.string)
////   use enrolled <- decode.field("enrolled", decode.bool)
////   decode.success(Player(name:, score:, colour:, enrolled:))
//// }
////
//// let result = decode.run(decoder, data)
////
//// assert result == Ok(Player("Mel Smith", 180, "Red", True))
//// ```
////
//// The ordering of the parameters defined with the `parameter` function must match
//// the ordering of the decoders used with the `field` function.
////
//// ## Enum variants
////
//// Imagine you have a custom type where all the variants do not contain any values.
////
//// ```gleam
//// pub type PocketMonsterType {
////   Fire
////   Water
////   Grass
////   Electric
//// }
//// ```
////
//// You might choose to encode these variants as strings, `"fire"` for `Fire`,
//// `"water"` for `Water`, and so on. To decode them you'll need to decode the dynamic
//// data as a string, but then you'll need to decode it further still as not all
//// strings are valid values for the enum. This can be done with the `then`
//// function, which enables running a second decoder after the first one
//// succeeds.
////
//// ```gleam
//// let decoder = {
////   use decoded_string <- zero.then(zero.string)
////   case decoded_string {
////     // Return succeeding decoders for valid strings
////     "fire" -> zero.success(Fire)
////     "water" -> zero.success(Water)
////     "grass" -> zero.success(Grass)
////     "electric" -> zero.success(Electric)
////     // Return a failing decoder for any other strings
////     _ -> zero.failure(Fire, "PocketMonsterType")
////   }
//// }
////
//// let result = zero.run(decoder, dynamic.from("water"))
//// assert result == Ok(Water)
////
//// let result = zero.run(decoder, dynamic.from("wobble"))
//// assert result == Error([DecodeError("PocketMonsterType", "String", [])])
//// ```
////
//// ## Record variants
////
//// Decoding type variants that contain other values is done by combining the
//// techniques from the "enum variants" and "records" examples. Imagine you have
//// this custom type that you want to decode:
////
//// ```gleam
//// pub type PocketMonsterPerson {
////   Trainer(name: String, badge_count: Int)
////   GymLeader(name: String, speciality: PocketMonsterType)
//// }
//// ```
//// And you would like to be able to decode these from dynamic data like this:
//// ```erlang
//// {
////   "type" -> "trainer",
////   "name" -> "Ash",
////   "badge-count" -> 1,
//// }
//// ```
//// ```erlang
//// {
////   "type" -> "gym-leader",
////   "name" -> "Misty",
////   "speciality" -> "water",
//// }
//// ```
////
//// Notice how both documents have a `"type"` field, which is used to indicate which
//// variant the data is for.
////
//// First, define decoders for each of the variants:
////
//// ```gleam
//// let trainer_decoder = {
////   use name <- decode.field("name", decode.string)
////   use badge_count <- decode.field("badge-count", decode.int)
////   decode.success(Trainer(name, badge_count))
//// })
////
//// let gym_leader_decoder = {
////   use name <- decode.field("name", decode.string)
////   use speciality <- decode.field("speciality", pocket_monster_type_decoder)
////   decode.success(GymLeader(name, speciality))
//// }
//// ```
////
//// A third decoder can be used to extract and decode the `"type"` field, and the
//// `then` function then returns whichever decoder is suitable for the document.
////
//// ```gleam
//// let decoder = {
////   use tag <- zero.field("type", zero.string)
////   case tag {
////     "gym-leader" -> gym_leader_decoder
////     _ -> trainer_decoder
////   }
//// }
////
//// decode.run(decoder, data)
//// ```

import gleam/dict.{type Dict}
import gleam/dynamic.{type DecodeError, type Dynamic, DecodeError}
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result

/// A decoder is a value that can be used to turn dynamically typed `Dynamic`
/// data into typed data using the `from` function.
///
/// Several smaller decoders can be combined to make larger decoders using
/// functions such as `list` and `field`.
///
pub opaque type Decoder(t) {
  Decoder(function: fn(Dynamic) -> #(t, List(dynamic.DecodeError)))
}

/// The same as [`field`](#field), except taking a path to the value rather
/// than a field name.
///
/// # Examples
///
/// ```gleam
/// let data = dynamic.from(dict.from_list([
///   #("data", data.from_list([
///     #("email", "lucy@example.com"),
///     #("name", "Lucy"),
///   ]))
/// ]))
///
/// decode.into({
///   use name <- decode.parameter
///   use email <- decode.parameter
///   SignUp(name: name, email: email)
/// })
/// |> decode.subfield(["data", "name"], string)
/// |> decode.subfield(["data", "email"], string)
/// |> decode.run(data)
/// // -> Ok(SignUp(name: "Lucy", email: "lucy@example.com"))
/// ```
///
pub fn subfield(
  field_path: List(name),
  field_decoder: Decoder(t),
  next: fn(t) -> Decoder(final),
) -> Decoder(final) {
  Decoder(function: fn(data) {
    let #(out, errors1) = index(field_path, [], field_decoder.function, data)
    let #(out, errors2) = next(out).function(data)
    #(out, list.append(errors1, errors2))
  })
}

pub fn run(
  decoder: Decoder(t),
  data: Dynamic,
) -> Result(t, List(dynamic.DecodeError)) {
  let #(maybe_invalid_data, errors) = decoder.function(data)
  case errors {
    [] -> Ok(maybe_invalid_data)
    _ -> Error(errors)
  }
}

pub fn at(path: List(segment), inner: Decoder(a)) -> Decoder(a) {
  Decoder(function: fn(data) { index(path, [], inner.function, data) })
}

fn index(
  path: List(a),
  position: List(a),
  inner: fn(Dynamic) -> #(b, List(dynamic.DecodeError)),
  data: Dynamic,
) -> #(b, List(dynamic.DecodeError)) {
  case path {
    [] -> {
      inner(data)
      |> push_path(list.reverse(position))
    }

    [key, ..path] -> {
      case bare_index(data, key) {
        Ok(data) -> {
          index(path, [key, ..position], inner, data)
        }
        Error(kind) -> {
          let #(default, _) = inner(data)
          #(default, [DecodeError(kind, dynamic.classify(data), [])])
          |> push_path(list.reverse(position))
        }
      }
    }
  }
}

@external(erlang, "decode_ffi", "index")
@external(javascript, "../decode_ffi.mjs", "index")
fn bare_index(data: Dynamic, key: anything) -> Result(Dynamic, String)

fn push_path(
  layer: #(t, List(DecodeError)),
  path: List(key),
) -> #(t, List(DecodeError)) {
  let decoder =
    dynamic.any([
      dynamic.string,
      fn(x) { result.map(dynamic.int(x), int.to_string) },
    ])
  let path =
    list.map(path, fn(key) {
      let key = dynamic.from(key)
      case decoder(key) {
        Ok(key) -> key
        Error(_) -> "<" <> dynamic.classify(key) <> ">"
      }
    })
  let errors =
    list.map(layer.1, fn(error) {
      DecodeError(..error, path: list.append(path, error.path))
    })
  #(layer.0, errors)
}

pub fn success(data: t) -> Decoder(t) {
  Decoder(function: fn(_) { #(data, []) })
}

pub fn decode_error(
  expected expected: String,
  found found: Dynamic,
) -> List(dynamic.DecodeError) {
  [DecodeError(expected:, found: dynamic.classify(found), path: [])]
}

pub fn field(
  field_name: name,
  field_decoder: Decoder(t),
  next: fn(t) -> Decoder(final),
) -> Decoder(final) {
  subfield([field_name], field_decoder, next)
}

fn run_dynamic_function(
  data: Dynamic,
  zero: t,
  f: dynamic.Decoder(t),
) -> #(t, List(dynamic.DecodeError)) {
  case f(data) {
    Ok(data) -> #(data, [])
    Error(errors) -> #(zero, errors)
  }
}

pub const string: Decoder(String) = Decoder(decode_string)

fn decode_string(data: Dynamic) -> #(String, List(dynamic.DecodeError)) {
  run_dynamic_function(data, "", dynamic.string)
}

pub const bool: Decoder(Bool) = Decoder(decode_bool)

fn decode_bool(data: Dynamic) -> #(Bool, List(dynamic.DecodeError)) {
  run_dynamic_function(data, False, dynamic.bool)
}

pub const int: Decoder(Int) = Decoder(decode_int)

fn decode_int(data: Dynamic) -> #(Int, List(dynamic.DecodeError)) {
  run_dynamic_function(data, 0, dynamic.int)
}

pub const float: Decoder(Float) = Decoder(decode_float)

fn decode_float(data: Dynamic) -> #(Float, List(dynamic.DecodeError)) {
  run_dynamic_function(data, 0.0, dynamic.float)
}

pub const dynamic: Decoder(Dynamic) = Decoder(decode_dynamic)

fn decode_dynamic(data: Dynamic) -> #(Dynamic, List(dynamic.DecodeError)) {
  #(data, [])
}

pub const bit_array: Decoder(BitArray) = Decoder(decode_bit_array)

fn decode_bit_array(data: Dynamic) -> #(BitArray, List(dynamic.DecodeError)) {
  run_dynamic_function(data, <<>>, dynamic.bit_array)
}

pub fn list(of inner: Decoder(a)) -> Decoder(List(a)) {
  Decoder(fn(data) {
    decode_list(data, inner.function, fn(p, k) { push_path(p, [k]) }, 0, [])
  })
}

@external(erlang, "decode_ffi", "list")
@external(javascript, "../decode_ffi.mjs", "list")
fn decode_list(
  data: Dynamic,
  item: fn(Dynamic) -> #(t, List(dynamic.DecodeError)),
  push_path: fn(#(t, List(DecodeError)), key) -> #(t, List(DecodeError)),
  index: Int,
  acc: List(t),
) -> #(List(t), List(dynamic.DecodeError))

pub fn dict(
  key: Decoder(key),
  value: Decoder(value),
) -> Decoder(Dict(key, value)) {
  Decoder(fn(data) {
    case decode_dict(data) {
      Error(_) -> #(dict.new(), decode_error("Dict", data))
      Ok(dict) ->
        dict.fold(dict, #(dict.new(), []), fn(a, k, v) {
          // If there are any errors from previous key-value pairs then we
          // don't need to run the decoders, instead return the existing acc.
          case a.1 {
            [] -> fold_dict(a, k, v, key.function, value.function)
            _ -> a
          }
        })
    }
  })
}

fn fold_dict(
  acc: #(Dict(k, v), List(dynamic.DecodeError)),
  key: Dynamic,
  value: Dynamic,
  key_decoder: fn(Dynamic) -> #(k, List(dynamic.DecodeError)),
  value_decoder: fn(Dynamic) -> #(v, List(dynamic.DecodeError)),
) -> #(Dict(k, v), List(dynamic.DecodeError)) {
  // First we decode the key.
  case key_decoder(key) {
    #(key, []) ->
      // Then we decode the value.
      case value_decoder(value) {
        #(value, []) -> {
          // It worked! Insert the new key-value pair so we can move onto the next.
          let dict = dict.insert(acc.0, key, value)
          #(dict, acc.1)
        }
        #(_, errors) -> push_path(#(dict.new(), errors), ["values"])
      }
    #(_, errors) -> push_path(#(dict.new(), errors), ["keys"])
  }
}

@external(erlang, "decode_ffi", "dict")
@external(javascript, "../decode_ffi.mjs", "dict")
fn decode_dict(data: Dynamic) -> Result(Dict(Dynamic, Dynamic), Nil)

pub fn optional(inner: Decoder(a)) -> Decoder(Option(a)) {
  Decoder(function: fn(data) {
    case dynamic.optional(Ok)(data) {
      Ok(option.None) -> #(option.None, [])
      Ok(option.Some(data)) -> {
        let #(data, errors) = inner.function(data)
        #(option.Some(data), errors)
      }
      Error(_) -> {
        let #(data, errors) = inner.function(data)
        #(option.Some(data), errors)
      }
    }
  })
}

pub fn map(decoder: Decoder(a), transformer: fn(a) -> b) -> Decoder(b) {
  Decoder(function: fn(d) {
    let #(data, errors) = decoder.function(d)
    #(transformer(data), errors)
  })
}

pub fn map_errors(
  decoder: Decoder(a),
  transformer: fn(List(DecodeError)) -> List(DecodeError),
) -> Decoder(a) {
  Decoder(function: fn(d) {
    let #(data, errors) = decoder.function(d)
    #(data, transformer(errors))
  })
}

pub fn collapse_errors(decoder: Decoder(a), name: String) -> Decoder(a) {
  Decoder(function: fn(dynamic_data) {
    let #(data, errors) as layer = decoder.function(dynamic_data)
    case errors {
      [] -> layer
      _ -> #(data, decode_error(name, dynamic_data))
    }
  })
}

pub fn then(decoder: Decoder(a), next: fn(a) -> Decoder(b)) -> Decoder(b) {
  Decoder(function: fn(dynamic_data) {
    let #(data, errors) = decoder.function(dynamic_data)
    let decoder = next(data)
    let #(data, _) as layer = decoder.function(dynamic_data)
    case errors {
      [] -> layer
      _ -> #(data, errors)
    }
  })
}

/// Create a new decoder from several other decoders. Each of the inner
/// decoders is run in turn, and the value from the first to succeed is used.
///
/// If no decoder succeeds then the errors from the first decoder is used.
/// If you wish for different errors then you may wish to use the
/// `collapse_errors` or `map_errors` functions.
///
/// # Examples
///
/// ```gleam
/// decode.one_of(decode.string, or: [
///   decode.int |> decode.map(int.to_string),
///   decode.float |> decode.map(float.to_string),
/// ])
/// |> decode.from(dynamic.from(1000))
/// // -> Ok("1000")
/// ```
///
pub fn one_of(
  first: Decoder(a),
  or alternatives: List(Decoder(a)),
) -> Decoder(a) {
  Decoder(function: fn(dynamic_data) {
    let #(_, errors) as layer = first.function(dynamic_data)
    case errors {
      [] -> layer
      _ -> run_decoders(dynamic_data, layer, alternatives)
    }
  })
}

fn run_decoders(
  data: Dynamic,
  failure: #(a, List(DecodeError)),
  decoders: List(Decoder(a)),
) -> #(a, List(DecodeError)) {
  case decoders {
    [] -> failure

    [decoder, ..decoders] -> {
      let #(_, errors) as layer = decoder.function(data)
      case errors {
        [] -> layer
        _ -> run_decoders(data, failure, decoders)
      }
    }
  }
}

/// Define a decoder that always fails. The parameter for this function is the
/// name of the type that has failed to decode.
///
pub fn failure(zero: a, expected: String) -> Decoder(a) {
  Decoder(function: fn(d) { #(zero, decode_error(expected, d)) })
}
