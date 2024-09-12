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
//// be used to describe or construct them. In these examples the JSON syntax is
//// largely used, and you can apply the same techniques to data from any source.
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
////   |> decode.from(data)
////
//// let assert Ok("Hello, Joe!") = result
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
////   |> decode.from(data)
////
//// let assert Ok([1, 2, 3, 4]) = result
//// ```
////
//// On Erlang this decoder can decode from lists, and on JavaScript it can decode
//// from lists as well as JavaScript arrays.
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
////   |> decode.from(data)
////
//// let assert Ok(option.Some(12.45)) = result
//// ```
//// ```gleam
//// // Data:
//// // null
////
//// let result =
////   decode.optional(decode.int)
////   |> decode.from(data)
////
//// let assert Ok(option.None) = result
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
//// // { "Lucy": 10, "Nubi": 20 }
////
//// let result =
////   decode.dict(decode.string, decode.int)
////   |> decode.from(data)
////
//// let assert Ok(dict.from_list([#("Lucy", 10), #("Nubi": 20)])) = result
//// ```
////
//// ## Indexing objects
////
//// The [`at`](#at) decoder can be used to decode a value that is nested within
//// key-value containers such as Gleam dicts, Erlang maps, or JavaScript objects.
////
//// ```gleam
//// // Data:
//// // { "one": { "two": 123 } }
////
//// let result =
////   decode.at(["one", "two"], decode.int)
////   |> decode.from(data)
////
//// let assert Ok(123) = result
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
////   |> decode.from(data)
////
//// let assert Ok("two") = result
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
//// //   "score": 180,
//// //   "name": "Mel Smith",
//// //   "is-admin": false,
//// //   "enrolled": true,
//// //   "colour": "Red",
//// // }
////
//// let result =
////   decode.into({
////     use name <- decode.parameter
////     use score <- decode.parameter
////     use colour <- decode.parameter
////     use enrolled <- decode.parameter
////     Player(name: name, score: score, colour: colour, enrolled: enrolled)
////   })
////   |> decode.field("name", decode.string)
////   |> decode.field("score", decode.int)
////   |> decode.field("colour", decode.string)
////   |> decode.field("enrolled", decode.bool)
////   |> decode.from(data)
////
//// let assert Ok(Player("Mel Smith", 180, "Red", True)) = result
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
//// let decoder =
////   decode.string
////   |> decode.then(fn(decoded_string) {
////     case decoded_string {
////       // Return succeeding decoders for valid strings
////       "fire" -> decode.into(Fire)
////       "water" -> decode.into(Water)
////       "grass" -> decode.into(Grass)
////       "electric" -> decode.into(Electric)
////       // Return a failing decoder for any other strings
////       _ -> decode.fail("PocketMonsterType")
////     }
////   })
////
//// decoder
//// |> decode.from(dynamic.from("water"))
//// // -> Ok(Water)
////
//// decoder
//// |> decode.from(dynamic.from("wobble"))
//// // -> Error([DecodeError("PocketMonsterType", "String", [])])
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
//// And you would like to be able to decode these from JSON documents like these.
//// ```json
//// {
////   "type": "trainer",
////   "name": "Ash",
////   "badge-count": 1,
//// }
//// ```
//// ```json
//// {
////   "type": "gym-leader",
////   "name": "Misty",
////   "speciality": "water",
//// }
//// ```
////
//// Notice how both documents have a `"type"` field, which is used to indicate which
//// variant the data is for.
////
//// First, define decoders for each of the variants:
////
//// ```gleam
//// let trainer_decoder =
////   decode.into({
////     use name <- decode.parameter
////     use badge_count <- decode.parameter
////     Trainer(name, badge_count)
////   })
////   |> decode.field("name", decode.string)
////   |> decode.field("badge-count", decode.int)
////
//// let gym_leader_decoder =
////   decode.into({
////     use name <- decode.parameter
////     use speciality <- decode.parameter
////     GymLeader(name, speciality)
////   })
////   |> decode.field("name", decode.string)
////   |> decode.field("speciality", pocket_monster_type_decoder)
//// ```
////
//// A third decoder can be used to extract and decode the `"type"` field, and the
//// `then` function then returns whichever decoder is suitable for the document.
////
//// ```gleam
//// let decoder =
////   decode.at(["type"], decode.string)
////   |> decode.then(fn(tag) {
////     case tag {
////       "trainer" -> trainer_decoder
////       "gym-leader" -> gym_leader
////       _ -> decode.fail("PocketMonsterPerson")
////     }
////   })
////
//// decoder
//// |> decode.from(data)
//// ```

import gleam/dict.{type Dict}
import gleam/dynamic.{type DecodeError, type Dynamic, DecodeError}
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result

/// The result that a decoder returns when run.
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
/// made with `use name <- decode.parameter`, then the decoder can be used with
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
/// // -> Ok(SignUp(name: "Lucy", email: "lucy@example.com"))
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
/// // -> Ok(SignUp(name: "Lucy", email: "lucy@example.com"))
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
///
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
/// // -> Ok(SignUp(name: "Lucy", email: "lucy@example.com"))
/// ```
///
/// If you wish to decode a value that is more deeply nested within the dynamic
/// data, see [`subfield`](#subfield) and [`at`](#at).
///
pub fn field(
  decoder: Decoder(fn(t1) -> t2),
  field_name: name,
  field_decoder: Decoder(t1),
) -> Decoder(t2) {
  subfield(decoder, [field_name], field_decoder)
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
/// |> decode.from(data)
/// // -> Ok(SignUp(name: "Lucy", email: "lucy@example.com"))
/// ```
///
pub fn subfield(
  decoder: Decoder(fn(t1) -> t2),
  field_path: List(name),
  field_decoder: Decoder(t1),
) -> Decoder(t2) {
  Decoder(continuation: fn(data) {
    let constructor = decoder.continuation(data)
    let data = from(at(field_path, field_decoder), data)
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
/// decode.string
/// |> decode.from(dynamic.from("Hello!"))
/// // -> Ok("Hello!")
/// ```
///
pub const string: Decoder(String) = Decoder(continuation: dynamic.string)

/// A decoder that decodes `Bool` values.
///
/// # Examples
///
/// ```gleam
/// decode.bool
/// |> decode.from(dynamic.from(True))
/// // -> Ok(True)
/// ```
///
pub const bool: Decoder(Bool) = Decoder(continuation: dynamic.bool)

/// A decoder that decodes `Int` values.
///
/// # Examples
///
/// ```gleam
/// decode.int
/// |> decode.from(dynamic.from(147))
/// // -> Ok(147)
/// ```
///
pub const int: Decoder(Int) = Decoder(continuation: dynamic.int)

/// A decoder that decodes `Float` values.
///
/// # Examples
///
/// ```gleam
/// decode.float
/// |> decode.from(dynamic.from(3.14))
/// // -> Ok(3.14)
/// ```
///
pub const float: Decoder(Float) = Decoder(continuation: dynamic.float)

/// A decoder that decodes `Dynamic` values. This decoder never returns an error.
///
/// # Examples
///
/// ```gleam
/// decode.dynamic
/// |> decode.from(dynamic.from(3.14))
/// // -> Ok(dynamic.from(3.14))
/// ```
///
pub const dynamic: Decoder(Dynamic) = Decoder(continuation: Ok)

/// A decoder that decodes `BitArray` values. This decoder never returns an error.
///
/// # Examples
///
/// ```gleam
/// decode.bit_array
/// |> decode.from(dynamic.from(<<5, 7>>))
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
/// decode.list(of: decode.int)
/// |> decode.from(dynamic.from([1, 2, 3]))
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
/// decode.dict(decode.string, decode.int)
/// |> decode.from(dynamic.from(values))
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
/// This function can handle common representations of null on all runtimes, such as
/// `nil`, `null`, and `undefined` on Erlang, and `undefined` and `null` on
/// JavaScript.
///
/// # Examples
///
/// ```gleam
/// decode.optional(decode.int)
/// |> decode.from(dynamic.from(100))
/// // -> Ok(option.Some(100))
/// ```
///
/// ```gleam
/// decode.optional(decode.int)
/// |> decode.from(dynamic.from(Nil))
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
/// decode.at(["one", "two"], decode.int)
/// |> decode.from(data)
/// // -> Ok(1000)
/// ```
///
/// ```gleam
/// decode.optional(decode.int)
/// |> decode.from(dynamic.from(Nil))
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
/// decode.int
/// |> decode.map(int.to_string)
/// |> decode.from(dynamic.from(1000))
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
/// This function may be useful if you wish to simplify errors before
/// presenting them to a user, particularly when using the `one_of` function.
///
/// # Examples
///
/// ```gleam
/// decode.string
/// |> decode.collapse_errors("MyThing")
/// |> decode.from(dynamic.from(1000))
/// // -> Error([DecodeError("MyThing", "Int", [])])
/// ```
///
pub fn collapse_errors(decoder: Decoder(a), name: String) -> Decoder(a) {
  Decoder(continuation: fn(d) {
    case decoder.continuation(d) {
      Ok(a) -> Ok(a)
      Error(_) -> Error([DecodeError(name, dynamic.classify(d), [])])
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

/// Create a new decoder from several other decoders. Each of the inner
/// decoders is run in turn, and the value from the first to succeed is used.
///
/// If no decoder succeeds then the errors from the final decoder is used.
/// If you wish for different errors then you may wish to use the
/// `collapse_errors` or `map_errors` functions.
///
/// # Examples
///
/// ```gleam
/// decode.one_of([
///   decode.string,
///   decode.int |> decode.map(int.to_string),
/// ])
/// |> decode.from(dynamic.from(1000))
/// // -> Ok("1000")
/// ```
///
pub fn one_of(decoders: List(Decoder(a))) -> Decoder(a) {
  Decoder(continuation: fn(d) { run_decoders(d, decoders) })
}

fn run_decoders(data: Dynamic, decoders: List(Decoder(a))) -> DecodeResult(a) {
  case decoders {
    [] -> Error([DecodeError("nothing", dynamic.classify(data), [])])

    [decoder] -> from(decoder, data)

    [decoder, ..decoders] ->
      case from(decoder, data) {
        Ok(value) -> Ok(value)
        Error(_) -> run_decoders(data, decoders)
      }
  }
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

/// Define a decoder that always fails. The parameter for this function is the
/// name of the type that has failed to decode.
///
pub fn fail(expected: String) -> Decoder(a) {
  Decoder(continuation: fn(d) {
    Error([DecodeError(expected, dynamic.classify(d), [])])
  })
}
