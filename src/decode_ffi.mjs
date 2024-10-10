import { Ok, Error, List, NonEmpty } from "./gleam.mjs";
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

export function list(data, decode, pushPath, index, emptyList) {
  if (!(data instanceof List)) {
    throw "todo";
  }

  const decoded = [];

  for (const element of data) {
    const layer = decode(element);
    const [out, errors] = layer;

    if (errors instanceof NonEmpty) {
      const [_, errors] = pushPath(layer, index.toString());
      return [emptyList, errors];
    }
    decoded.push(out);
    index++;
  }

  return [List.fromArray(decoded), emptyList];
}

export function dict(data) {
  if (data instanceof Dict) {
    return new Ok(data);
  }
  return new Error();
}
