import { Ok, Error } from "./gleam.mjs";
import { default as Dict } from "../gleam_stdlib/dict.mjs";

export function index(data, key) {
  const int = Number.isInteger(key);

  if (data instanceof Dict || data instanceof WeakMap || data instanceof Map) {
    const entry = data.get(key, undefined);
    return new Ok(entry);
  }

  if (
    (int && Array.isArray(data)) ||
    (data && typeof data === "object") ||
    (data && Object.getPrototypeOf(data) === Object.prototype)
  ) {
    return new Ok(data[key]);
  }

  return new Error(int ? "Indexable" : "Dict");
}
