import decode
import gleam/dict
import gleam/dynamic
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
  |> decode.run(data)
  |> should.be_ok
  |> should.equal(User("Nubi", "nubi@example.com", False, True, 180))
}
