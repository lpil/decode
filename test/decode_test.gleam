import decode
import gleam/dict
import gleam/dynamic.{DecodeError}
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

pub fn shallow_list_ok_test() {
  let data = dynamic.from([dynamic.from(1), dynamic.from("two")])
  decode.shallow_list
  |> decode.from(data)
  |> should.be_ok
  |> should.equal([dynamic.from(1), dynamic.from("two")])
}

pub fn shallow_list_error_test() {
  let data = dynamic.from(123)
  decode.shallow_list
  |> decode.from(data)
  |> should.be_error
  |> should.equal([DecodeError("List", "Int", [])])
}
