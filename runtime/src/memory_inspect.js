const ENV_TAG = 0;
const CONST_TAG = 1;
const ARRAY_TAG = 2;
const TUPLE_TAG = 3;

const TAGGED_POINTER_BYTE_SIZE = 5;

const PARENT_ENV_POINTER_BYTE_SIZE = 4;
const ENV_COUNTER_BYTE_SIZE = 4;
const ENV_HEADER_BYTE_SIZE = 1 + PARENT_ENV_POINTER_BYTE_SIZE + ENV_COUNTER_BYTE_SIZE; // 9 bytes, 1 byte for tag, 4 bytes for parent env pointer, 4 bytes for env count.

function Constant(value, address) {
  return { type: "Const", value, address };
}

function TaggedPointer(tag, pointer, address) {
  return { type: "Pointer", tag, pointer, address };
}

function Tuple(variant, components, address) {
  return { type: "Tuple",  variant, components, address };
}

// TODO: May be useful during GC inspection.
function Moved(pointer) {
  return { type: "Moved", pointer };
}

function Array(byte_count, bytes, address) {
  return { type: "Array",  byte_count, bytes, address };
}

function Env(old_env_pointer, values, address) {
  return { type: "Env", old_env_pointer, values, address };
}

function readRawPointer(view, raw_pointer) {
  const tag = view.getInt8(raw_pointer);
  if (tag == CONST_TAG) {
    const value = view.getInt32(raw_pointer + 1, true);
    return Constant(value, raw_pointer);
  } else if (tag == TUPLE_TAG) {
    const tuple_pointer = view.getInt32(raw_pointer + 1, true);
    return TaggedPointer("Tuple", tuple_pointer, raw_pointer);
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
  return Tuple(variant, components, tuple_pointer);
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
  return Tuple(tuple.variant, components, tuple.address);
}
module.exports.deepReadTuple = deepReadTuple;

function readStack(view, raw_pointer_start, raw_pointer_end) {
  const stack = [];

  function readNextValue(raw_pointer) {
    if (raw_pointer == raw_pointer_end) {
      // Done
    } else {
      const tag = view.getInt8(raw_pointer);
      if (tag == CONST_TAG) {
        const value = view.getInt32(raw_pointer + 1, true);
        stack.push(Constant(value), raw_pointer);
        readNextValue(raw_pointer + TAGGED_POINTER_BYTE_SIZE);
      } else if (tag == TUPLE_TAG) {
        const tuple_pointer = view.getInt32(raw_pointer + 1, true);
        stack.push(TaggedPointer("Tuple", tuple_pointer, raw_pointer));
        readNextValue(raw_pointer + TAGGED_POINTER_BYTE_SIZE)
      } else if (tag == ENV_TAG) {
        const old_env_pointer = view.getInt32(raw_pointer + 1, true);
        const count = view.getInt32(raw_pointer + 1 + PARENT_ENV_POINTER_BYTE_SIZE, true);
        let next_arg_at = raw_pointer + ENV_HEADER_BYTE_SIZE;
        const values = [];
        for (let i = 0; i < count; i++) {
          const arg = readRawPointer(view, next_arg_at);
          values.push(arg);
          next_arg_at = next_arg_at + TAGGED_POINTER_BYTE_SIZE;
        }
        stack.push(Env(old_env_pointer, values, raw_pointer));
        readNextValue(raw_pointer + ENV_HEADER_BYTE_SIZE + count * TAGGED_POINTER_BYTE_SIZE);
      } else if (tag == ARRAY_TAG) {
        throw Error("Arrays not yet implemented");
      } else {
        throw Error(`Unknown value Tag ${tag}`);
      }
    }
  }

  readNextValue(raw_pointer_start);
  return stack;
}
module.exports.readStack = readStack;

function deepReadStack(view, raw_pointer_start, raw_pointer_end) {
  const stack = [];

  function readNextValue(raw_pointer) {
    if (raw_pointer == raw_pointer_end) {
      // Done
    } else {
      const tag = view.getInt8(raw_pointer);
      if (tag == CONST_TAG) {
        const value = view.getInt32(raw_pointer + 1, true);
        stack.push(Constant(value, raw_pointer));
        readNextValue(raw_pointer + TAGGED_POINTER_BYTE_SIZE);
      } else if (tag == TUPLE_TAG) {
        const tuple_pointer = view.getInt32(raw_pointer + 1, true);
        const tagged_pointer = TaggedPointer("Tuple", tuple_pointer);
        stack.push(deepReadTuple(view, tagged_pointer));
        readNextValue(raw_pointer + TAGGED_POINTER_BYTE_SIZE)
      } else if (tag == ENV_TAG) {
        const old_env_pointer = view.getInt32(raw_pointer + 1, true);
        const count = view.getInt32(raw_pointer + 1 + PARENT_ENV_POINTER_BYTE_SIZE, true);
        let next_arg_at = raw_pointer + ENV_HEADER_BYTE_SIZE;
        const values = [];
        for (let i = 0; i < count; i++) {
          const arg = deepReadRawPointer(view, next_arg_at);
          values.push(arg);
          next_arg_at = next_arg_at + TAGGED_POINTER_BYTE_SIZE;
        }
        stack.push(Env(old_env_pointer, values, raw_pointer));
        readNextValue(raw_pointer + ENV_HEADER_BYTE_SIZE + count * TAGGED_POINTER_BYTE_SIZE);
      } else if (tag == ARRAY_TAG) {
        throw Error("Arrays not yet implemented");
      } else {
        throw Error(`Unknown value Tag ${tag}`);
      }
    }
  }

  readNextValue(raw_pointer_start);
  return stack;
}
module.exports.deepReadStack = deepReadStack;

function showValue(x) {
  if (x.type == "Const") {
    return `Const[${x.value}]`;
  } else if (x.type == "Tuple") {
    const components_str = [];
    x.components.forEach(component => {
      components_str.push(showValue(component));
    });
    return `Tuple[${x.variant}](${components_str.join(', ')})`;
  } else if (x.type == "Env") {
    const values_str = [];
    x.values.forEach(value => {
      values_str.push(showValue(value));
    });
    return `Env[#${x.old_env_pointer}](${values_str.join(", ")})`;
  } else if (x.type == "Array") {
    throw Error("Arrays not yet implemented");
  } else {
    throw Error(`Unexpected value type in ${x}`);
  }
}
module.exports.showValue = showValue;

function showValueWithAddress(x) {
  if (x.type == "Const") {
    return `Const@${x.address}[${x.value}]`;
  } else if (x.type == "Tuple") {
    const components_str = [];
    x.components.forEach(component => {
      components_str.push(showValueWithAddress(component));
    });
    return `Tuple@${x.address}[${x.variant}](${components_str.join(', ')})`;
  } else if (x.type == "Env") {
    const values_str = [];
    x.values.forEach(value => {
      values_str.push(showValueWithAddress(value));
    });
    return `Env@${x.address}[#${x.old_env_pointer}](${values_str.join(", ")})`;
  } else if (x.type == "Array") {
    throw Error("Arrays not yet implemented");
  } else {
    throw Error(`Unexpected value type in ${x}`);
  }
}
module.exports.showValueWithAddress = showValueWithAddress;

function showStack(stack) {
  const strings = [];
  stack.forEach(x => {
    strings.push(showValue(x));
  });

  return `Nil <: ${strings.join(' <: ')}`;
}
module.exports.showStack = showStack;

function showStackWithAddress(stack) {
  const strings = [];
  stack.forEach(x => {
    strings.push(showValueWithAddress(x));
  });

  return `Nil <: ${strings.join(' <: ')}`;
}
module.exports.showStackWithAddress = showStackWithAddress;
