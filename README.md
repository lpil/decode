# decode

Ergonomic dynamic decoders for Gleam!

[![Package Version](https://img.shields.io/hexpm/v/decode)](https://hex.pm/packages/decode)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/decode/)

```sh
gleam add decode
```
```gleam
import decode

pub type User {
  User(name: String, email: String, is_admin: Bool)
}

/// Decode data of this shape into a `User` record.
///
/// {
///   "name": "Lucy",
///   "email": "lucy@example.com",
///   "is-admin": true
/// }
///
pub fn run(data: Dynamic) {
  let result =
    decode.into({
      use name <- decode.parameter
      use email <- decode.parameter
      use is_admin <- decode.parameter
      User(name, email, is_admin)
    })
    |> decode.field("name", decode.string)
    |> decode.field("email", decode.string)
    |> decode.field("is-admin", decode.bool)

  decoder
  |> decode.from(data)
}
```
For more documentation and examples view <https://hexdocs.pm/decode>.
