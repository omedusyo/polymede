const CONST_TAG = 1;
const ARRAY_TAG = 2;
const TUPLE_TAG = 3;

const TAGGED_POINTER_BYTE_SIZE = 5;

function Constant(value) {
  return { type: "Const", value };
}

function TaggedPointer(tag, pointer) {
  return { type: "Pointer", tag, pointer };
}

function Tuple(variant, components) {
  return { type: "Tuple",  variant, components };
}

function Env(old_env_pointer, values) {
  return { type: "Env", old_env_pointer, values };
}

function Stack(values) {
  return { type: "Stack", values };
}

function readRawPointer(view, raw_pointer) {
  const tag = view.getInt8(raw_pointer);
  if (tag == CONST_TAG) {
    const value = view.getInt32(raw_pointer + 1, true);
    return Constant(value);
  } else if (tag == TUPLE_TAG) {
    const tuple_pointer = view.getInt32(raw_pointer + 1, true);
    return TaggedPointer("Tuple", tuple_pointer);
  } else if (tag == ARRAY_TAG) {
    throw Error("Arrays not yet implemented");
  } else {
    throw Error(`Unknown value Tag ${tag}`);
  }
}
module.exports.readRawPointer = readRawPointer;

function readTuple(view, tagged_pointer) {
  if (tagged_pointer.tag != "Tuple") { throw Error(`Attempt to read non-tuple as tuple @ ${tagged_pointer}`) }
  const tuple_pointer = tagged_pointer.pointer;
  // 1 byte for GC, 4 bytes for variant, 1 byte for count, followed by components
  const variant_offset = 1;
  const count_offset = variant_offset + 4;
  const components_offset = count_offset + 1;

  const variant = view.getInt32(tuple_pointer + variant_offset, true);
  const count = view.getInt8(tuple_pointer + count_offset);

  const components = [];
  let next_component_raw_pointer = tuple_pointer + components_offset;
  for (let i = 0; i < count; i++) {
    const component = readRawPointer(view, next_component_raw_pointer);
    components.push(component);
    next_component_raw_pointer += TAGGED_POINTER_BYTE_SIZE;
  }
  return Tuple(variant, components);
}
module.exports.readTuple = readTuple;

// WARNING: Does not detect cyclic structure.
function deepReadRawPointer(view, raw_pointer) {
  const tagged_pointer = readRawPointer(view, raw_pointer);
  return deepReadTaggedPointer(view, tagged_pointer);
}
module.exports.deepReadRawPointer = deepReadRawPointer;

function deepReadTaggedPointer(view, tagged_pointer) {
  if (tagged_pointer.type == "Const") {
    return tagged_pointer;
  } else if (tagged_pointer.type == "Pointer"){
    if (tagged_pointer.tag == "Tuple") {
      return deepReadTuple(view, tagged_pointer);
    } else {
      throw Error(`We have a pointer that's not a tuple ${tagged_pointer}`);
    }
  } else {
    throw Error(`We have a tagged pointer that's neither a Const nor a Pointer ${tagged_pointer}`);
  }
}
module.exports.deepReadTaggedPointer = deepReadTaggedPointer;

function deepReadTuple(view, tagged_pointer) {
  const tuple = readTuple(view, tagged_pointer);
  const components = [];
  tuple.components.forEach(component => {
    components.push(deepReadTaggedPointer(view, component));
  });
  return Tuple(tuple.variant, components);
}
module.exports.deepReadTuple = deepReadTuple;

function showValue(x) {
  if (x.type == "Const") {
    return `Const[${x.value}]`;
  } else if (x.type == "Tuple") {
    const components_str = [];
    x.components.forEach(component => {
      components_str.push(showValue(component));
    });
    return `Tuple[${x.variant}](${components_str.join(', ')})`;
  } else {
    throw Error(`Unexpected value type in ${x}`);
  }
}
module.exports.showValue = showValue;
