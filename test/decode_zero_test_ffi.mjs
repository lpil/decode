export function object(items) {
  const object = {};
  for (const [k, v] of items) {
    object[k] = v;
  }
  return object;
}

export function map(items) {
  const object = new Map();
  for (const [k, v] of items) {
    object.set(k, v);
  }
  return object;
}
