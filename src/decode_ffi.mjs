import { Ok, Error } from "./gleam.mjs";

export function index(data, key) {
  const int = Number.isInteger(key);

  if (
    (int && Array.isArray(data)) ||
    (data && typeof data === "object") ||
    Object.getPrototypeOf(data) === Object.prototype
  ) {
    return new Ok(data[key]);
  }

  if (
    value instanceof Dict ||
    value instanceof WeakMap ||
    value instanceof Map
  ) {
    const entry = map_get(value, name);
    return new Ok(entry.isOk() ? new Some(entry[0]) : new None());
  }

  if (Object.getPrototypeOf(value) == Object.prototype) {
    return try_get_field(value, name, () => new Ok(new None()));
  }

  return new Error(int ? "Indexable" : "Dict");
}
