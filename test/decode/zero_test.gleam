import decode/zero
import gleam/dict
import gleam/dynamic.{type Dynamic, DecodeError}
import gleam/float
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

  let decoder = {
    use name <- zero.field("name", zero.string)
    use email <- zero.field("email", zero.string)
    use is_admin <- zero.field("is_admin", zero.bool)
    use is_confirmed <- zero.field("is_confirmed", zero.bool)
    use score <- zero.field("score", zero.int)
    zero.success(User(name:, email:, is_admin:, is_confirmed:, score:))
  }

  zero.run(data, decoder)
  |> should.be_ok
  |> should.equal(User("Nubi", "nubi@example.com", False, True, 180))
}

pub fn field_ok_test() {
  let data = dynamic.from(dict.from_list([#("name", dynamic.from("Nubi"))]))
  let decoder = {
    use name <- zero.field("name", zero.string)
    zero.success(name)
  }

  zero.run(data, decoder)
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

  zero.run(data, decoder)
  |> should.be_ok
  |> should.equal("Nubi")
}

pub fn field_int_index_ok_test() {
  let decoder = {
    use x <- zero.field(0, zero.string)
    use y <- zero.field(1, zero.string)
    zero.success(#(x, y))
  }

  dynamic.from(#("one", "two", "three"))
  |> zero.run(decoder)
  |> should.be_ok
  |> should.equal(#("one", "two"))
}

pub fn field_int_index_list_ok_test() {
  let decoder = {
    use x <- zero.field(0, zero.string)
    use y <- zero.field(1, zero.string)
    zero.success(#(x, y))
  }

  dynamic.from(["one", "two", "three", "four"])
  |> zero.run(decoder)
  |> should.be_ok
  |> should.equal(#("one", "two"))
}

pub fn subfield_not_found_error_test() {
  let decoder = {
    use name <- zero.subfield(["name"], zero.string)
    zero.success(name)
  }

  dynamic.from(123)
  |> zero.run(decoder)
  |> should.be_error
  |> should.equal([DecodeError("Dict", "Int", [])])
}

pub fn field_not_found_error_test() {
  let decoder = {
    use name <- zero.subfield(["name"], zero.string)
    zero.success(name)
  }

  dynamic.from(123)
  |> zero.run(decoder)
  |> should.be_error
  |> should.equal([DecodeError("Dict", "Int", [])])
}

pub fn field_wrong_inner_error_test() {
  let decoder = {
    use name <- zero.field("name", zero.string)
    zero.success(name)
  }

  dynamic.from(dict.from_list([#("name", dynamic.from(123))]))
  |> zero.run(decoder)
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["name"])])
}

pub fn subfield_int_index_ok_test() {
  let decoder = {
    use x <- zero.subfield([0, 1], zero.string)
    use y <- zero.subfield([1, 0], zero.string)
    zero.success(#(x, y))
  }

  dynamic.from(#(#("one", "two", "three"), #("a", "b")))
  |> zero.run(decoder)
  |> should.be_ok
  |> should.equal(#("two", "a"))
}

pub fn subfield_wrong_inner_error_test() {
  let data = dynamic.from(dict.from_list([#("name", dynamic.from(123))]))
  zero.run(data, {
    use name <- zero.field("name", zero.string)
    zero.success(name)
  })
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["name"])])
}

pub fn string_ok_test() {
  dynamic.from("Hello!")
  |> zero.run(zero.string)
  |> should.be_ok
  |> should.equal("Hello!")
}

pub fn string_error_test() {
  dynamic.from(123)
  |> zero.run(zero.string)
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", [])])
}

pub fn dynamic_test() {
  let data = dynamic.from(123)
  data
  |> zero.run(zero.dynamic)
  |> should.be_ok
  |> should.equal(data)
}

pub fn int_ok_test() {
  dynamic.from(123)
  |> zero.run(zero.int)
  |> should.be_ok
  |> should.equal(123)
}

pub fn int_error_test() {
  dynamic.from("123")
  |> zero.run(zero.int)
  |> should.be_error
  |> should.equal([DecodeError("Int", "String", [])])
}

pub fn float_ok_test() {
  dynamic.from(123.45)
  |> zero.run(zero.float)
  |> should.be_ok
  |> should.equal(123.45)
}

pub fn float_error_test() {
  dynamic.from("123.45")
  |> zero.run(zero.float)
  |> should.be_error
  |> should.equal([DecodeError("Float", "String", [])])
}

pub fn bool_true_test() {
  dynamic.from(True)
  |> zero.run(zero.bool)
  |> should.be_ok
  |> should.equal(True)
}

pub fn bool_false_test() {
  dynamic.from(False)
  |> zero.run(zero.bool)
  |> should.be_ok
  |> should.equal(False)
}

pub fn bool_error_test() {
  dynamic.from(123)
  |> zero.run(zero.bool)
  |> should.be_error
  |> should.equal([DecodeError("Bool", "Int", [])])
}

pub fn bit_array_ok_test() {
  dynamic.from(<<1, 5, 3>>)
  |> zero.run(zero.bit_array)
  |> should.be_ok
  |> should.equal(<<1, 5, 3>>)
}

pub fn bit_array_error_test() {
  dynamic.from(123)
  |> zero.run(zero.bit_array)
  |> should.be_error
  |> should.equal([DecodeError("BitArray", "Int", [])])
}

pub fn list_tuple_ok_test() {
  dynamic.from(#("Hello", "Joe"))
  |> zero.run(zero.list(zero.string))
  |> should.be_ok
  |> should.equal(["Hello", "Joe"])
}

pub fn list_string_ok_test() {
  dynamic.from(["Hello", "Joe"])
  |> zero.run(zero.list(zero.string))
  |> should.be_ok
  |> should.equal(["Hello", "Joe"])
}

pub fn list_bool_ok_test() {
  dynamic.from([True, False])
  |> zero.run(zero.list(zero.bool))
  |> should.be_ok
  |> should.equal([True, False])
}

pub fn list_error_test() {
  dynamic.from(123)
  |> zero.run(zero.list(zero.int))
  |> should.be_error
  |> should.equal([DecodeError("List", "Int", [])])
}

pub fn list_inner_0_error_test() {
  dynamic.from([1, 2])
  |> zero.run(zero.list(zero.string))
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["0"])])
}

pub fn list_inner_1_error_test() {
  dynamic.from([dynamic.from("1"), dynamic.from(2)])
  |> zero.run(zero.list(zero.string))
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["1"])])
}

pub fn list_tuple_inner_1_error_test() {
  dynamic.from(#("1", 2))
  |> zero.run(zero.list(zero.string))
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["1"])])
}

pub fn dict_ok_test() {
  let values = dict.from_list([#("first", 1), #("second", 2)])
  dynamic.from(values)
  |> zero.run(zero.dict(zero.string, zero.int))
  |> should.be_ok
  |> should.equal(values)
}

pub fn dict_value_error_test() {
  dynamic.from(dict.from_list([#(1.1, 1), #(1.2, 2)]))
  |> zero.run(zero.dict(zero.float, zero.string))
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["values"])])
}

pub fn dict_key_error_test() {
  dynamic.from(dict.from_list([#(1.1, 1), #(1.2, 2)]))
  |> zero.run(zero.dict(zero.string, zero.int))
  |> should.be_error
  |> should.equal([DecodeError("String", "Float", ["keys"])])
}

pub fn dict_error_test() {
  dynamic.from(123)
  |> zero.run(zero.dict(zero.string, zero.int))
  |> should.be_error
  |> should.equal([DecodeError("Dict", "Int", [])])
}

pub fn at_dict_string_ok_test() {
  dynamic.from(
    dict.from_list([
      #(
        "first",
        dict.from_list([#("second", dict.from_list([#("third", 1337)]))]),
      ),
    ]),
  )
  |> zero.run(zero.at(["first", "second", "third"], zero.int))
  |> should.be_ok
  |> should.equal(1337)
}

pub fn at_dict_int_ok_test() {
  dynamic.from(
    dict.from_list([
      #(10, dict.from_list([#(20, dict.from_list([#(30, 1337)]))])),
    ]),
  )
  |> zero.run(zero.at([10, 20, 30], zero.int))
  |> should.be_ok
  |> should.equal(1337)
}

pub fn at_tuple_int_ok_test() {
  dynamic.from(#("x", #("a", "b", "c"), "z"))
  |> zero.run(zero.at([1, 0], zero.string))
  |> should.be_ok
  |> should.equal("a")
}

pub fn at_wrong_inner_error_test() {
  dynamic.from(
    dict.from_list([
      #(
        "first",
        dict.from_list([#("second", dict.from_list([#("third", 1337)]))]),
      ),
    ]),
  )
  |> zero.run(zero.at(["first", "second", "third"], zero.string))
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["first", "second", "third"])])
}

pub fn at_no_path_error_test() {
  dynamic.from(dict.from_list([#("first", dict.from_list([#("third", 1337)]))]))
  |> zero.run(zero.at(["first", "second", "third"], zero.int))
  |> should.be_error
  |> should.equal([DecodeError("Field", "Nothing", ["first", "second"])])
}

pub fn optional_string_present_ok_test() {
  dynamic.from("Hello, Joe!")
  |> zero.run(zero.optional(zero.string))
  |> should.be_ok
  |> should.equal(option.Some("Hello, Joe!"))
}

pub fn optional_bool_present_ok_test() {
  dynamic.from(True)
  |> zero.run(zero.optional(zero.bool))
  |> should.be_ok
  |> should.equal(option.Some(True))
}

pub fn optional_bool_absent_nil_ok_test() {
  dynamic.from(Nil)
  |> zero.run(zero.optional(zero.bool))
  |> should.be_ok
  |> should.equal(option.None)
}

pub fn optional_bool_absent_none_ok_test() {
  dynamic.from(option.None)
  |> zero.run(zero.optional(zero.bool))
  |> should.be_ok
  |> should.equal(option.None)
}

pub fn optional_error_test() {
  dynamic.from(123)
  |> zero.run(zero.optional(zero.string))
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", [])])
}

pub fn map_test() {
  dynamic.from(123)
  |> zero.run(zero.int |> zero.map(int.to_string))
  |> should.be_ok
  |> should.equal("123")
}

pub fn map_errors_test() {
  let decoder =
    zero.at(
      ["data"],
      zero.map_errors(zero.string, fn(errors) {
        let assert [DecodeError("String", "Int", [])] = errors
        [
          DecodeError("Wibble", "Wobble", ["ok"]),
          DecodeError("Wabble", "Wubble", ["ok"]),
        ]
      }),
    )

  dynamic.from(dict.from_list([#("data", 123)]))
  |> zero.run(decoder)
  |> should.be_error
  |> should.equal([
    DecodeError("Wibble", "Wobble", ["data", "ok"]),
    DecodeError("Wabble", "Wubble", ["data", "ok"]),
  ])
}

pub fn collapse_errors_test() {
  dynamic.from(dict.from_list([#("data", 123)]))
  |> zero.run(zero.at(["data"], zero.string |> zero.collapse_errors("Wibble")))
  |> should.be_error
  |> should.equal([DecodeError("Wibble", "Int", ["data"])])
}

pub fn then_test() {
  let decoder =
    zero.at(["key"], zero.int)
    |> zero.then(fn(i) {
      zero.at(["value"], case i {
        1 -> zero.int |> zero.map(AnInt)
        _ -> zero.string |> zero.map(AString)
      })
    })

  dynamic.from(dict.from_list([#("key", 1), #("value", 100)]))
  |> zero.run(decoder)
  |> should.be_ok
  |> should.equal(AnInt(100))

  dynamic.from(
    dict.from_list([#("key", dynamic.from(2)), #("value", dynamic.from("Hi!"))]),
  )
  |> zero.run(decoder)
  |> should.be_ok
  |> should.equal(AString("Hi!"))
}

type IntOrString {
  AnInt(Int)
  AString(String)
}

pub fn then_error_0_test() {
  let decoder =
    zero.at(["key"], zero.int)
    |> zero.then(fn(i) {
      zero.at(["value"], case i {
        1 -> zero.int |> zero.map(AnInt)
        _ -> zero.string |> zero.map(AString)
      })
    })

  dynamic.from(123)
  |> zero.run(decoder)
  |> should.be_error
  |> should.equal([DecodeError("Dict", "Int", [])])
}

pub fn then_error_1_test() {
  let decoder =
    zero.at(["key"], zero.int)
    |> zero.then(fn(i) {
      zero.at(["value"], case i {
        1 -> zero.int |> zero.map(AnInt)
        _ -> zero.string |> zero.map(AString)
      })
    })

  dynamic.from(
    dict.from_list([#("key", dynamic.from(1)), #("value", dynamic.from("Hi!"))]),
  )
  |> zero.run(decoder)
  |> should.be_error
  |> should.equal([DecodeError("Int", "String", ["value"])])
}

pub type MyEnum {
  A
  B
  C
}

pub fn then_enum_test() {
  let decoder =
    zero.string
    |> zero.then(fn(s) {
      case s {
        "a" -> zero.success(A)
        "b" -> zero.success(B)
        "c" -> zero.success(C)
        _ -> zero.failure(A, "MyEnum")
      }
    })

  zero.run(dynamic.from("a"), decoder)
  |> should.be_ok
  |> should.equal(A)

  zero.run(dynamic.from("b"), decoder)
  |> should.be_ok
  |> should.equal(B)

  zero.run(dynamic.from("c"), decoder)
  |> should.be_ok
  |> should.equal(C)

  zero.run(dynamic.from("d"), decoder)
  |> should.be_error
  |> should.equal([DecodeError("MyEnum", "String", [])])
}

pub fn one_of_ok_0_test() {
  dynamic.from("Hello!")
  |> zero.run(zero.one_of(zero.string, [zero.int |> zero.map(int.to_string)]))
  |> should.be_ok
  |> should.equal("Hello!")
}

pub fn one_of_ok_1_test() {
  let decoder =
    zero.one_of(zero.string, [
      zero.int
      |> zero.map(int.to_string),
    ])
  dynamic.from(123)
  |> zero.run(decoder)
  |> should.be_ok
  |> should.equal("123")
}

pub fn one_of_ok_2_test() {
  let decoder =
    zero.one_of(zero.string, [
      zero.int |> zero.map(int.to_string),
      zero.float |> zero.map(float.to_string),
    ])
  dynamic.from(12.45)
  |> zero.run(decoder)
  |> should.be_ok
  |> should.equal("12.45")
}

pub fn one_of_error_test() {
  let decoder =
    zero.one_of(zero.string, or: [
      zero.int
      |> zero.map(int.to_string),
    ])
  dynamic.from(1.2)
  |> zero.run(decoder)
  |> should.be_error
  |> should.equal([DecodeError("String", "Float", [])])
}

pub fn failure_test() {
  dynamic.from(123)
  |> zero.run(zero.failure(1, "WibbleWobble"))
  |> should.be_error
  |> should.equal([DecodeError("WibbleWobble", "Int", [])])
}

pub fn variants_test() {
  let decoder = {
    use tag <- zero.field("tag", zero.string)
    case tag {
      "int" -> {
        use int <- zero.field("the-int", zero.int)
        zero.success(AnInt(int))
      }
      "string" -> {
        use string <- zero.field("the-string", zero.string)
        zero.success(AString(string))
      }
      _ -> {
        zero.failure(AnInt(0), "IntOrString")
      }
    }
  }

  // Int variant
  dynamic.from(
    dict.from_list([
      #("tag", dynamic.from("int")),
      #("the-int", dynamic.from(123)),
    ]),
  )
  |> zero.run(decoder)
  |> should.be_ok
  |> should.equal(AnInt(123))

  // String variant
  dynamic.from(
    dict.from_list([
      #("tag", dynamic.from("string")),
      #("the-string", dynamic.from("hello")),
    ]),
  )
  |> zero.run(decoder)
  |> should.be_ok
  |> should.equal(AString("hello"))

  // Invalid tag
  dynamic.from(
    dict.from_list([
      #("tag", dynamic.from("dunno")),
      #("the-string", dynamic.from("hello")),
    ]),
  )
  |> zero.run(decoder)
  |> should.be_error
  |> should.equal([DecodeError("IntOrString", "Dict", [])])

  // Missing tag
  dynamic.from(dict.from_list([#("the-string", dynamic.from("hello"))]))
  |> zero.run(decoder)
  |> should.be_error
  |> should.equal([
    DecodeError("Field", "Nothing", ["tag"]),
    DecodeError("IntOrString", "Dict", []),
  ])

  // String invalid field
  dynamic.from(
    dict.from_list([
      #("tag", dynamic.from("string")),
      #("the-string", dynamic.from(12.3)),
    ]),
  )
  |> zero.run(decoder)
  |> should.be_error
  |> should.equal([DecodeError("String", "Float", ["the-string"])])
}

pub type PocketMonsterType {
  Fire
  Water
  Grass
  Electric
}

pub fn documentation_enum_example_test() {
  let decoder = {
    use decoded_string <- zero.then(zero.string)
    case decoded_string {
      // Return succeeding decoders for valid strings
      "fire" -> zero.success(Fire)
      "water" -> zero.success(Water)
      "grass" -> zero.success(Grass)
      "electric" -> zero.success(Electric)
      // Return a failing decoder for any other strings
      _ -> zero.failure(Fire, "PocketMonsterType")
    }
  }

  zero.run(dynamic.from("water"), decoder)
  |> should.be_ok
  |> should.equal(Water)

  zero.run(dynamic.from("wobble"), decoder)
  |> should.be_error
  |> should.equal([DecodeError("PocketMonsterType", "String", [])])
}

pub type PocketMonsterPerson {
  Trainer(name: String, badge_count: Int)
  GymLeader(name: String, speciality: String)
}

pub fn documentation_variants_example_test() {
  let trainer_decoder = {
    use name <- zero.field("name", zero.string)
    use badge_count <- zero.field("badge-count", zero.int)
    zero.success(Trainer(name, badge_count))
  }

  let gym_leader_decoder = {
    use name <- zero.field("name", zero.string)
    use speciality <- zero.field("speciality", zero.string)
    zero.success(GymLeader(name, speciality))
  }

  let decoder = {
    use tag <- zero.field("type", zero.string)
    case tag {
      "gym-leader" -> gym_leader_decoder
      _ -> trainer_decoder
    }
  }

  // Trainer
  dynamic.from(
    dict.from_list([
      #("type", dynamic.from("trainer")),
      #("name", dynamic.from("Ash")),
      #("badge-count", dynamic.from(8)),
    ]),
  )
  |> zero.run(decoder)
  |> should.be_ok
  |> should.equal(Trainer("Ash", 8))

  // Gym leader
  dynamic.from(
    dict.from_list([
      #("type", dynamic.from("gym-leader")),
      #("name", dynamic.from("Brock")),
      #("speciality", dynamic.from("Rock")),
    ]),
  )
  |> zero.run(decoder)
  |> should.be_ok
  |> should.equal(GymLeader("Brock", "Rock"))

  // Error
  dynamic.from(
    dict.from_list([
      #("type", dynamic.from("gym-leader")),
      #("name", dynamic.from("Brock")),
    ]),
  )
  |> zero.run(decoder)
  |> should.be_error
  |> should.equal([
    DecodeError(expected: "Field", found: "Nothing", path: ["speciality"]),
  ])
}

pub fn new_primitive_decoder_string_ok_test() {
  dynamic.from("Hello!")
  |> zero.run(zero.new_primitive_decoder(dynamic.string, ""))
  |> should.be_ok
  |> should.equal("Hello!")
}

pub fn new_primitive_decoder_string_error_test() {
  dynamic.from(123)
  |> zero.run(zero.new_primitive_decoder(dynamic.string, ""))
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", [])])
}

pub fn new_primitive_decoder_float_ok_test() {
  dynamic.from(12.4)
  |> zero.run(zero.new_primitive_decoder(dynamic.float, 0.0))
  |> should.be_ok
  |> should.equal(12.4)
}

pub fn new_primitive_decoder_float_error_test() {
  dynamic.from("blah")
  |> zero.run(zero.new_primitive_decoder(dynamic.float, 0.0))
  |> should.be_error
  |> should.equal([DecodeError("Float", "String", [])])
}

pub type LinkedList {
  ListEmpty
  ListNonEmpty(element: Int, tail: LinkedList)
}

pub fn list_decoder() -> zero.Decoder(LinkedList) {
  use tag <- zero.field("type", zero.string)
  case tag {
    "list-non-empty" -> {
      use element <- zero.field("element", zero.int)
      use tail <- zero.field("tail", list_decoder())
      zero.success(ListNonEmpty(element:, tail:))
    }
    _ -> zero.success(ListEmpty)
  }
}

pub fn recursive_data_structure_test() {
  dynamic.from(
    dict.from_list([
      #("type", dynamic.from("list-non-empty")),
      #("element", dynamic.from(1)),
      #(
        "tail",
        dynamic.from(
          dict.from_list([
            #("type", dynamic.from("list-non-empty")),
            #("element", dynamic.from(2)),
            #(
              "tail",
              dynamic.from(
                dict.from_list([#("type", dynamic.from("list-empty"))]),
              ),
            ),
          ]),
        ),
      ),
    ]),
  )
  |> zero.run(list_decoder())
  |> should.be_ok
  |> should.equal(ListNonEmpty(1, ListNonEmpty(2, ListEmpty)))
}

pub fn optionally_at_dict_string_ok_test() {
  dynamic.from(
    dict.from_list([
      #(
        "first",
        dict.from_list([#("second", dict.from_list([#("third", 1337)]))]),
      ),
    ]),
  )
  |> zero.run(zero.optionally_at(["first", "second", "third"], 100, zero.int))
  |> should.be_ok
  |> should.equal(1337)
}

pub fn optionally_at_dict_int_ok_test() {
  dynamic.from(
    dict.from_list([
      #(10, dict.from_list([#(20, dict.from_list([#(30, 1337)]))])),
    ]),
  )
  |> zero.run(zero.optionally_at([10, 20, 30], 123, zero.int))
  |> should.be_ok
  |> should.equal(1337)
}

pub fn optionally_at_tuple_int_ok_test() {
  dynamic.from(#("x", #("a", "b", "c"), "z"))
  |> zero.run(zero.optionally_at([1, 0], "something", zero.string))
  |> should.be_ok
  |> should.equal("a")
}

pub fn optionally_at_wrong_inner_error_test() {
  dynamic.from(
    dict.from_list([
      #(
        "first",
        dict.from_list([#("second", dict.from_list([#("third", 1337)]))]),
      ),
    ]),
  )
  |> zero.run(zero.optionally_at(
    ["first", "second", "third"],
    "default",
    zero.string,
  ))
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["first", "second", "third"])])
}

pub fn optionally_at_no_path_error_test() {
  dynamic.from(dict.from_list([#("first", dict.from_list([#("third", 1337)]))]))
  |> zero.run(zero.optionally_at(["first", "second", "third"], 100, zero.int))
  |> should.be_ok
  |> should.equal(100)
}

@external(erlang, "maps", "from_list")
@external(javascript, "../decode_zero_test_ffi.mjs", "object")
fn make_object(items: List(#(String, t))) -> Dynamic

@external(erlang, "maps", "from_list")
@external(javascript, "../decode_zero_test_ffi.mjs", "map")
fn make_map(items: List(#(String, t))) -> Dynamic

pub fn js_object_test() {
  [#("a", 10), #("b", 20), #("c", 30)]
  |> make_object
  |> zero.run(zero.dict(zero.string, zero.int))
  |> should.be_ok
  |> should.equal(dict.from_list([#("a", 10), #("b", 20), #("c", 30)]))
}

pub fn js_map_test() {
  [#("a", 10), #("b", 20), #("c", 30)]
  |> make_map
  |> zero.run(zero.dict(zero.string, zero.int))
  |> should.be_ok
  |> should.equal(dict.from_list([#("a", 10), #("b", 20), #("c", 30)]))
}
