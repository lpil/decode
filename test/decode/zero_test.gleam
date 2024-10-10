import decode/zero
import gleam/dict
import gleam/dynamic.{DecodeError}
import gleam/int
import gleam/option
import gleeunit/should

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

  {
    use name <- zero.field("name", zero.string)
    use email <- zero.field("email", zero.string)
    use is_admin <- zero.field("is_admin", zero.bool)
    use is_confirmed <- zero.field("is_confirmed", zero.bool)
    use score <- zero.field("score", zero.int)
    zero.success(User(name:, email:, is_admin:, is_confirmed:, score:))
  }
  |> zero.run(data)
  |> should.be_ok
  |> should.equal(User("Nubi", "nubi@example.com", False, True, 180))
}

pub fn field_ok_test() {
  let data = dynamic.from(dict.from_list([#("name", dynamic.from("Nubi"))]))
  let decoder = {
    use name <- zero.field("name", zero.string)
    zero.success(name)
  }
  zero.run(decoder, data)
  |> should.be_ok
  |> should.equal("Nubi")
}

pub fn subfield_ok_test() {
  let data =
    dynamic.from(
      dict.from_list([
        #("person", dict.from_list([#("name", dynamic.from("Nubi"))])),
      ]),
    )
  let decoder = {
    use name <- zero.subfield(["person", "name"], zero.string)
    zero.success(name)
  }
  zero.run(decoder, data)
  |> should.be_ok
  |> should.equal("Nubi")
}

pub fn field_int_index_ok_test() {
  let data = dynamic.from(#("one", "two", "three"))
  {
    use x <- zero.field(0, zero.string)
    use y <- zero.field(1, zero.string)
    zero.success(#(x, y))
  }
  |> zero.run(data)
  |> should.be_ok
  |> should.equal(#("one", "two"))
}

pub fn subfield_not_found_error_test() {
  let data = dynamic.from(123)
  let decoder = {
    use name <- zero.subfield(["name"], zero.string)
    zero.success(name)
  }
  zero.run(decoder, data)
  |> should.be_error
  |> should.equal([DecodeError("Dict", "Int", [])])
}

pub fn field_not_found_error_test() {
  let data = dynamic.from(123)
  let decoder = {
    use name <- zero.subfield(["name"], zero.string)
    zero.success(name)
  }
  zero.run(decoder, data)
  |> should.be_error
  |> should.equal([DecodeError("Dict", "Int", [])])
}

pub fn field_wrong_inner_error_test() {
  let data = dynamic.from(dict.from_list([#("name", dynamic.from(123))]))
  {
    use name <- zero.field("name", zero.string)
    zero.success(name)
  }
  |> zero.run(data)
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["name"])])
}

pub fn subfield_int_index_ok_test() {
  let data = dynamic.from(#(#("one", "two", "three"), #("a", "b")))
  {
    use x <- zero.subfield([0, 1], zero.string)
    use y <- zero.subfield([1, 0], zero.string)
    zero.success(#(x, y))
  }
  |> zero.run(data)
  |> should.be_ok
  |> should.equal(#("two", "a"))
}

pub fn subfield_wrong_inner_error_test() {
  let data = dynamic.from(dict.from_list([#("name", dynamic.from(123))]))
  {
    use name <- zero.field("name", zero.string)
    zero.success(name)
  }
  |> zero.run(data)
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["name"])])
}

pub fn string_ok_test() {
  let data = dynamic.from("Hello!")
  zero.string
  |> zero.run(data)
  |> should.be_ok
  |> should.equal("Hello!")
}

pub fn string_error_test() {
  let data = dynamic.from(123)
  zero.string
  |> zero.run(data)
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", [])])
}

pub fn dynamic_test() {
  let data = dynamic.from(123)
  zero.dynamic
  |> zero.run(data)
  |> should.be_ok
  |> should.equal(data)
}

pub fn int_ok_test() {
  let data = dynamic.from(123)
  zero.int
  |> zero.run(data)
  |> should.be_ok
  |> should.equal(123)
}

pub fn int_error_test() {
  let data = dynamic.from("123")
  zero.int
  |> zero.run(data)
  |> should.be_error
  |> should.equal([DecodeError("Int", "String", [])])
}

pub fn float_ok_test() {
  let data = dynamic.from(123.45)
  zero.float
  |> zero.run(data)
  |> should.be_ok
  |> should.equal(123.45)
}

pub fn float_error_test() {
  let data = dynamic.from("123.45")
  zero.float
  |> zero.run(data)
  |> should.be_error
  |> should.equal([DecodeError("Float", "String", [])])
}

pub fn bool_true_test() {
  let data = dynamic.from(True)
  zero.bool
  |> zero.run(data)
  |> should.be_ok
  |> should.equal(True)
}

pub fn bool_false_test() {
  let data = dynamic.from(False)
  zero.bool
  |> zero.run(data)
  |> should.be_ok
  |> should.equal(False)
}

pub fn bool_error_test() {
  let data = dynamic.from(123)
  zero.bool
  |> zero.run(data)
  |> should.be_error
  |> should.equal([DecodeError("Bool", "Int", [])])
}

pub fn bit_array_ok_test() {
  let data = dynamic.from(<<1, 5, 3>>)
  zero.bit_array
  |> zero.run(data)
  |> should.be_ok
  |> should.equal(<<1, 5, 3>>)
}

pub fn bit_array_error_test() {
  let data = dynamic.from(123)
  zero.bit_array
  |> zero.run(data)
  |> should.be_error
  |> should.equal([DecodeError("BitArray", "Int", [])])
}

pub fn list_string_ok_test() {
  let data = dynamic.from(["Hello", "Joe"])
  zero.list(zero.string)
  |> zero.run(data)
  |> should.be_ok
  |> should.equal(["Hello", "Joe"])
}

pub fn list_bool_ok_test() {
  let data = dynamic.from([True, False])
  zero.list(zero.bool)
  |> zero.run(data)
  |> should.be_ok
  |> should.equal([True, False])
}

pub fn list_error_test() {
  let data = dynamic.from(123)
  zero.list(zero.int)
  |> zero.run(data)
  |> should.be_error
  |> should.equal([DecodeError("List", "Int", [])])
}

pub fn list_inner_0_error_test() {
  let data = dynamic.from([1, 2])
  zero.list(zero.string)
  |> zero.run(data)
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["0"])])
}

pub fn list_inner_1_error_test() {
  let data = dynamic.from([dynamic.from("1"), dynamic.from(2)])
  zero.list(zero.string)
  |> zero.run(data)
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["1"])])
}

pub fn dict_ok_test() {
  let values = dict.from_list([#("first", 1), #("second", 2)])
  let data = dynamic.from(values)
  zero.dict(zero.string, zero.int)
  |> zero.run(data)
  |> should.be_ok
  |> should.equal(values)
}

pub fn dict_value_error_test() {
  let data = dynamic.from(dict.from_list([#(1.1, 1), #(1.2, 2)]))
  zero.dict(zero.float, zero.string)
  |> zero.run(data)
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["values"])])
}

pub fn dict_key_error_test() {
  let data = dynamic.from(dict.from_list([#(1.1, 1), #(1.2, 2)]))
  zero.dict(zero.string, zero.int)
  |> zero.run(data)
  |> should.be_error
  |> should.equal([DecodeError("String", "Float", ["keys"])])
}

pub fn dict_error_test() {
  let data = dynamic.from(123)
  zero.dict(zero.string, zero.int)
  |> zero.run(data)
  |> should.be_error
  |> should.equal([DecodeError("Dict", "Int", [])])
}
//
// pub fn at_dict_string_ok_test() {
//   let data =
//     dynamic.from(
//       dict.from_list([
//         #(
//           "first",
//           dict.from_list([#("second", dict.from_list([#("third", 1337)]))]),
//         ),
//       ]),
//     )
//   zero.at(["first", "second", "third"], zero.int)
//   |> zero.run(data)
//   |> should.be_ok
//   |> should.equal(1337)
// }
//
// pub fn at_dict_int_ok_test() {
//   let data =
//     dynamic.from(
//       dict.from_list([
//         #(10, dict.from_list([#(20, dict.from_list([#(30, 1337)]))])),
//       ]),
//     )
//   zero.at([10, 20, 30], zero.int)
//   |> zero.run(data)
//   |> should.be_ok
//   |> should.equal(1337)
// }
//
// pub fn at_tuple_int_ok_test() {
//   let data = dynamic.from(#("x", #("a", "b", "c"), "z"))
//   zero.at([1, 0], zero.string)
//   |> zero.run(data)
//   |> should.be_ok
//   |> should.equal("a")
// }
//
// pub fn at_wrong_inner_error_test() {
//   let data =
//     dynamic.from(
//       dict.from_list([
//         #(
//           "first",
//           dict.from_list([#("second", dict.from_list([#("third", 1337)]))]),
//         ),
//       ]),
//     )
//   zero.at(["first", "second", "third"], zero.string)
//   |> zero.run(data)
//   |> should.be_error
//   |> should.equal([DecodeError("String", "Int", ["first", "second", "third"])])
// }
//
// pub fn at_no_path_error_test() {
//   let data =
//     dynamic.from(
//       dict.from_list([#("first", dict.from_list([#("third", 1337)]))]),
//     )
//   zero.at(["first", "second", "third"], zero.int)
//   |> zero.run(data)
//   |> should.be_error
//   |> should.equal([DecodeError("Dict", "Nil", ["first", "second"])])
// }
//
// pub fn optional_string_present_ok_test() {
//   let data = dynamic.from("Hello, Joe!")
//   zero.optional(zero.string)
//   |> zero.run(data)
//   |> should.be_ok
//   |> should.equal(option.Some("Hello, Joe!"))
// }
//
// pub fn optional_bool_present_ok_test() {
//   let data = dynamic.from(True)
//   zero.optional(zero.bool)
//   |> zero.run(data)
//   |> should.be_ok
//   |> should.equal(option.Some(True))
// }
//
// pub fn optional_bool_absent_nil_ok_test() {
//   let data = dynamic.from(Nil)
//   zero.optional(zero.bool)
//   |> zero.run(data)
//   |> should.be_ok
//   |> should.equal(option.None)
// }
//
// pub fn optional_bool_absent_none_ok_test() {
//   let data = dynamic.from(option.None)
//   zero.optional(zero.bool)
//   |> zero.run(data)
//   |> should.be_ok
//   |> should.equal(option.None)
// }
//
// pub fn optional_error_test() {
//   let data = dynamic.from(123)
//   zero.optional(zero.string)
//   |> zero.run(data)
//   |> should.be_error
//   |> should.equal([DecodeError("String", "Int", [])])
// }
//
// pub fn map_test() {
//   let data = dynamic.from(123)
//   zero.int
//   |> zero.map(int.to_string)
//   |> zero.run(data)
//   |> should.be_ok
//   |> should.equal("123")
// }
//
// pub fn map_errors_test() {
//   let data = dynamic.from(dict.from_list([#("data", 123)]))
//   zero.at(
//     ["data"],
//     zero.map_errors(zero.string, fn(errors) {
//       let assert [DecodeError("String", "Int", [])] = errors
//       [
//         DecodeError("Wibble", "Wobble", ["ok"]),
//         DecodeError("Wabble", "Wubble", ["ok"]),
//       ]
//     }),
//   )
//   |> zero.run(data)
//   |> should.be_error
//   |> should.equal([
//     DecodeError("Wibble", "Wobble", ["data", "ok"]),
//     DecodeError("Wabble", "Wubble", ["data", "ok"]),
//   ])
// }
//
// pub fn collapse_errors_test() {
//   let data = dynamic.from(dict.from_list([#("data", 123)]))
//   zero.at(["data"], zero.string |> zero.collapse_errors("Wibble"))
//   |> zero.run(data)
//   |> should.be_error
//   |> should.equal([DecodeError("Wibble", "Int", ["data"])])
// }
//
// pub fn then_test() {
//   let decoder =
//     zero.at(["key"], zero.int)
//     |> zero.then(fn(i) {
//       zero.at(["value"], case i {
//         1 -> zero.int |> zero.map(AnInt)
//         _ -> zero.string |> zero.map(AString)
//       })
//     })
//
//   decoder
//   |> zero.run(dynamic.from(dict.from_list([#("key", 1), #("value", 100)])))
//   |> should.be_ok
//   |> should.equal(AnInt(100))
//
//   decoder
//   |> zero.run(
//     dynamic.from(
//       dict.from_list([
//         #("key", dynamic.from(2)),
//         #("value", dynamic.from("Hi!")),
//       ]),
//     ),
//   )
//   |> should.be_ok
//   |> should.equal(AString("Hi!"))
// }
//
// type IntOrString {
//   AnInt(Int)
//   AString(String)
// }
//
// pub fn then_error_0_test() {
//   let decoder =
//     zero.at(["key"], zero.int)
//     |> zero.then(fn(i) {
//       zero.at(["value"], case i {
//         1 -> zero.int |> zero.map(AnInt)
//         _ -> zero.string |> zero.map(AString)
//       })
//     })
//
//   decoder
//   |> zero.run(dynamic.from(123))
//   |> should.be_error
//   |> should.equal([DecodeError("Dict", "Int", [])])
// }
//
// pub fn then_error_1_test() {
//   let decoder =
//     zero.at(["key"], zero.int)
//     |> zero.then(fn(i) {
//       zero.at(["value"], case i {
//         1 -> zero.int |> zero.map(AnInt)
//         _ -> zero.string |> zero.map(AString)
//       })
//     })
//
//   decoder
//   |> zero.run(
//     dynamic.from(
//       dict.from_list([
//         #("key", dynamic.from(1)),
//         #("value", dynamic.from("Hi!")),
//       ]),
//     ),
//   )
//   |> should.be_error
//   |> should.equal([DecodeError("Int", "String", ["value"])])
// }
//
// pub type MyEnum {
//   A
//   B
//   C
// }
//
// pub fn then_enum_test() {
//   let decoder =
//     zero.string
//     |> zero.then(fn(s) {
//       case s {
//         "a" -> zero.into(A)
//         "b" -> zero.into(B)
//         "c" -> zero.into(C)
//         _ -> zero.fail("MyEnum")
//       }
//     })
//
//   zero.run(decoder, dynamic.from("a"))
//   |> should.be_ok
//   |> should.equal(A)
//
//   zero.run(decoder, dynamic.from("b"))
//   |> should.be_ok
//   |> should.equal(B)
//
//   zero.run(decoder, dynamic.from("c"))
//   |> should.be_ok
//   |> should.equal(C)
//
//   zero.run(decoder, dynamic.from("d"))
//   |> should.be_error
//   |> should.equal([DecodeError("MyEnum", "String", [])])
// }
//
// pub fn one_of_ok_0_test() {
//   zero.one_of([
//     zero.string,
//     zero.int
//       |> zero.map(int.to_string),
//   ])
//   |> zero.run(dynamic.from("Hello!"))
//   |> should.be_ok
//   |> should.equal("Hello!")
// }
//
// pub fn one_of_ok_1_test() {
//   zero.one_of([
//     zero.string,
//     zero.int
//       |> zero.map(int.to_string),
//   ])
//   |> zero.run(dynamic.from(123))
//   |> should.be_ok
//   |> should.equal("123")
// }
//
// pub fn one_of_error_test() {
//   zero.one_of([
//     zero.string,
//     zero.int
//       |> zero.map(int.to_string),
//   ])
//   |> zero.run(dynamic.from(1.2))
//   |> should.be_error
//   |> should.equal([DecodeError("Int", "Float", [])])
// }
//
// pub fn fail_test() {
//   zero.fail("WibbleWobble")
//   |> zero.run(dynamic.from(123))
//   |> should.be_error
//   |> should.equal([DecodeError("WibbleWobble", "Int", [])])
// }
