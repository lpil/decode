import decode
import gleam/dict
import gleam/dynamic.{DecodeError}
import gleam/int
import gleam/option
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub type User {
  User(
    name: String,
    email: String,
    is_admin: Bool,
    is_confirmed: Bool,
    score: Int,
  )
}

pub fn decoder_test() {
  let data =
    dynamic.from(
      dict.from_list([
        #("name", dynamic.from("Nubi")),
        #("email", dynamic.from("nubi@example.com")),
        #("is_admin", dynamic.from(False)),
        #("is_confirmed", dynamic.from(True)),
        #("score", dynamic.from(180)),
      ]),
    )

  let result =
    decode.into({
      use name <- decode.parameter
      use email <- decode.parameter
      use is_admin <- decode.parameter
      use is_confirmed <- decode.parameter
      use score <- decode.parameter
      User(name, email, is_admin, is_confirmed, score)
    })
    |> decode.field("name", decode.string)
    |> decode.field("email", decode.string)
    |> decode.field("is_admin", decode.bool)
    |> decode.field("is_confirmed", decode.bool)
    |> decode.field("score", decode.int)
    |> decode.from(data)

  result
  |> should.be_ok
  |> should.equal(User("Nubi", "nubi@example.com", False, True, 180))
}

pub fn field_ok_test() {
  let data = dynamic.from(dict.from_list([#("name", dynamic.from("Nubi"))]))
  let result =
    decode.into(fn(x) { x })
    |> decode.field("name", decode.string)
    |> decode.from(data)
  result
  |> should.be_ok
  |> should.equal("Nubi")
}

pub fn field_not_found_error_test() {
  let data = dynamic.from(123)
  let result =
    decode.into(fn(x) { x })
    |> decode.field("name", decode.string)
    |> decode.from(data)
  result
  |> should.be_error
  |> should.equal([DecodeError("Dict", "Int", [])])
}

pub fn string_ok_test() {
  let data = dynamic.from("Hello!")
  decode.string
  |> decode.from(data)
  |> should.be_ok
  |> should.equal("Hello!")
}

pub fn string_error_test() {
  let data = dynamic.from(123)
  decode.string
  |> decode.from(data)
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", [])])
}

pub fn dynamic_test() {
  let data = dynamic.from(123)
  decode.dynamic
  |> decode.from(data)
  |> should.be_ok
  |> should.equal(data)
}

pub fn int_ok_test() {
  let data = dynamic.from(123)
  decode.int
  |> decode.from(data)
  |> should.be_ok
  |> should.equal(123)
}

pub fn int_error_test() {
  let data = dynamic.from("123")
  decode.int
  |> decode.from(data)
  |> should.be_error
  |> should.equal([DecodeError("Int", "String", [])])
}

pub fn float_ok_test() {
  let data = dynamic.from(123.45)
  decode.float
  |> decode.from(data)
  |> should.be_ok
  |> should.equal(123.45)
}

pub fn float_error_test() {
  let data = dynamic.from("123.45")
  decode.float
  |> decode.from(data)
  |> should.be_error
  |> should.equal([DecodeError("Float", "String", [])])
}

pub fn bool_true_test() {
  let data = dynamic.from(True)
  decode.bool
  |> decode.from(data)
  |> should.be_ok
  |> should.equal(True)
}

pub fn bool_false_test() {
  let data = dynamic.from(False)
  decode.bool
  |> decode.from(data)
  |> should.be_ok
  |> should.equal(False)
}

pub fn bool_error_test() {
  let data = dynamic.from(123)
  decode.bool
  |> decode.from(data)
  |> should.be_error
  |> should.equal([DecodeError("Bool", "Int", [])])
}

pub fn bit_array_ok_test() {
  let data = dynamic.from(<<1, 5, 3>>)
  decode.bit_array
  |> decode.from(data)
  |> should.be_ok
  |> should.equal(<<1, 5, 3>>)
}

pub fn bit_array_error_test() {
  let data = dynamic.from(123)
  decode.bit_array
  |> decode.from(data)
  |> should.be_error
  |> should.equal([DecodeError("BitArray", "Int", [])])
}

pub fn list_string_ok_test() {
  let data = dynamic.from(["Hello", "Joe"])
  decode.list(decode.string)
  |> decode.from(data)
  |> should.be_ok
  |> should.equal(["Hello", "Joe"])
}

pub fn list_bool_ok_test() {
  let data = dynamic.from([True, False])
  decode.list(decode.bool)
  |> decode.from(data)
  |> should.be_ok
  |> should.equal([True, False])
}

pub fn list_error_test() {
  let data = dynamic.from(123)
  decode.list(decode.int)
  |> decode.from(data)
  |> should.be_error
  |> should.equal([DecodeError("List", "Int", [])])
}

pub fn dict_ok_test() {
  let values = dict.from_list([#("first", 1), #("second", 2)])
  let data = dynamic.from(values)
  decode.dict(decode.string, decode.int)
  |> decode.from(data)
  |> should.be_ok
  |> should.equal(values)
}

pub fn dict_value_error_test() {
  let data = dynamic.from(dict.from_list([#(1.1, 1), #(1.2, 2)]))
  decode.dict(decode.float, decode.string)
  |> decode.from(data)
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["values"])])
}

pub fn dict_key_error_test() {
  let data = dynamic.from(dict.from_list([#(1.1, 1), #(1.2, 2)]))
  decode.dict(decode.string, decode.int)
  |> decode.from(data)
  |> should.be_error
  |> should.equal([DecodeError("String", "Float", ["keys"])])
}

pub fn dict_error_test() {
  let data = dynamic.from(123)
  decode.dict(decode.string, decode.int)
  |> decode.from(data)
  |> should.be_error
  |> should.equal([DecodeError("Dict", "Int", [])])
}

pub fn at_dict_string_ok_test() {
  let data =
    dynamic.from(
      dict.from_list([
        #(
          "first",
          dict.from_list([#("second", dict.from_list([#("third", 1337)]))]),
        ),
      ]),
    )
  decode.at(["first", "second", "third"], decode.int)
  |> decode.from(data)
  |> should.be_ok
  |> should.equal(1337)
}

pub fn at_dict_int_ok_test() {
  let data =
    dynamic.from(
      dict.from_list([
        #(10, dict.from_list([#(20, dict.from_list([#(30, 1337)]))])),
      ]),
    )
  decode.at([10, 20, 30], decode.int)
  |> decode.from(data)
  |> should.be_ok
  |> should.equal(1337)
}

pub fn at_tuple_int_ok_test() {
  let data = dynamic.from(#("x", #("a", "b", "c"), "z"))
  decode.at([1, 0], decode.string)
  |> decode.from(data)
  |> should.be_ok
  |> should.equal("a")
}

pub fn at_wrong_inner_error_test() {
  let data =
    dynamic.from(
      dict.from_list([
        #(
          "first",
          dict.from_list([#("second", dict.from_list([#("third", 1337)]))]),
        ),
      ]),
    )
  decode.at(["first", "second", "third"], decode.string)
  |> decode.from(data)
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["first", "second", "third"])])
}

pub fn at_no_path_error_test() {
  let data =
    dynamic.from(
      dict.from_list([#("first", dict.from_list([#("third", 1337)]))]),
    )
  decode.at(["first", "second", "third"], decode.int)
  |> decode.from(data)
  |> should.be_error
  |> should.equal([DecodeError("Dict", "Nil", ["first", "second"])])
}

pub fn optional_string_present_ok_test() {
  let data = dynamic.from("Hello, Joe!")
  decode.optional(decode.string)
  |> decode.from(data)
  |> should.be_ok
  |> should.equal(option.Some("Hello, Joe!"))
}

pub fn optional_bool_present_ok_test() {
  let data = dynamic.from(True)
  decode.optional(decode.bool)
  |> decode.from(data)
  |> should.be_ok
  |> should.equal(option.Some(True))
}

pub fn optional_bool_absent_nil_ok_test() {
  let data = dynamic.from(Nil)
  decode.optional(decode.bool)
  |> decode.from(data)
  |> should.be_ok
  |> should.equal(option.None)
}

pub fn optional_bool_absent_none_ok_test() {
  let data = dynamic.from(option.None)
  decode.optional(decode.bool)
  |> decode.from(data)
  |> should.be_ok
  |> should.equal(option.None)
}

pub fn optional_error_test() {
  let data = dynamic.from(123)
  decode.optional(decode.string)
  |> decode.from(data)
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", [])])
}

pub fn map_test() {
  let data = dynamic.from(123)
  decode.int
  |> decode.map(int.to_string)
  |> decode.from(data)
  |> should.be_ok
  |> should.equal("123")
}

pub fn then_test() {
  let decoder =
    decode.at(["key"], decode.int)
    |> decode.then(fn(i) {
      decode.at(["value"], case i {
        1 -> decode.int |> decode.map(AnInt)
        _ -> decode.string |> decode.map(AString)
      })
    })

  decoder
  |> decode.from(dynamic.from(dict.from_list([#("key", 1), #("value", 100)])))
  |> should.be_ok
  |> should.equal(AnInt(100))

  decoder
  |> decode.from(
    dynamic.from(
      dict.from_list([
        #("key", dynamic.from(2)),
        #("value", dynamic.from("Hi!")),
      ]),
    ),
  )
  |> should.be_ok
  |> should.equal(AString("Hi!"))
}

type IntOrString {
  AnInt(Int)
  AString(String)
}

pub fn then_error_0_test() {
  let decoder =
    decode.at(["key"], decode.int)
    |> decode.then(fn(i) {
      decode.at(["value"], case i {
        1 -> decode.int |> decode.map(AnInt)
        _ -> decode.string |> decode.map(AString)
      })
    })

  decoder
  |> decode.from(dynamic.from(123))
  |> should.be_error
  |> should.equal([DecodeError("Dict", "Int", [])])
}

pub fn then_error_1_test() {
  let decoder =
    decode.at(["key"], decode.int)
    |> decode.then(fn(i) {
      decode.at(["value"], case i {
        1 -> decode.int |> decode.map(AnInt)
        _ -> decode.string |> decode.map(AString)
      })
    })

  decoder
  |> decode.from(
    dynamic.from(
      dict.from_list([
        #("key", dynamic.from(1)),
        #("value", dynamic.from("Hi!")),
      ]),
    ),
  )
  |> should.be_error
  |> should.equal([DecodeError("Int", "String", ["value"])])
}
