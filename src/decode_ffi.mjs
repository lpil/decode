import { Ok, Error } from "./gleam.mjs";

export function index(data, key) {
  if (
    Number.isInteger(key) &&
    Array.isArray(data) &&
    key >= 0 &&
    key < data.length
  ) {
    return new Ok(data[key]);
  }
  return new Error(undefined);
}
