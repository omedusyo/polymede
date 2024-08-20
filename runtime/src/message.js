// constant
// float
// string (what about string slices? no... we won't allow those, will be too complicated...)
// tuple
export const Tag = {
  Constant: 0,
  Float: 1,
  String: 2,
  Tuple: 3,
};

export const Value = {
  Constant(value) { return { tag: Tag.Constant, value } },
  Float(value) { return { tag: Tag.Float, value } },
  String(value) { return { tag: Tag.String, value } },
  Tuple(variant, args) { return { tag: Tag.Tuple, variant, args } },
};
window.Value = Value;

