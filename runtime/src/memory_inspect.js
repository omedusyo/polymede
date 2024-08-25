const ENV_TAG = 0;
const CONST_TAG = 1;
const TUPLE_TAG = 2;
const BYTE_ARRAY_TAG = 3;
const BYTE_ARRAY_SLICE_TAG = 4;

const TAGGED_POINTER_BYTE_SIZE = 5;

const PARENT_ENV_POINTER_BYTE_SIZE = 4;
const ENV_COUNTER_BYTE_SIZE = 4;
const ENV_HEADER_BYTE_SIZE = 1 + PARENT_ENV_POINTER_BYTE_SIZE + ENV_COUNTER_BYTE_SIZE; // 9 bytes, 1 byte for tag, 4 bytes for parent env pointer, 4 bytes for env count.


// | gc 1 byte | tag 1 byte | count 4 byte | string contents |
const BYTE_ARRAY_HEADER_BYTE_SIZE = 6;
const BYTE_ARRAY_COUNT_OFFSET = 2;
const BYTE_ARRAY_CONTENTS_OFFSET = 6;
const BYTE_ARRAY_COUNT_SIZE = 4;
// | gc 1 byte | tag 1 byte | 1st pointer 4 byte | 2nd pointer 4 byte | count 4 byte |
const SLICE_BYTE_SIZE = 14;
const SLICE_PARENT_POINTER_OFFSET = 2;
const SLICE_POINTER_OFFSET = 6;
const SLICE_COUNT_OFFSET = 10;
const SLICE_PARENT_POINTER_SIZE = 4;
const SLICE_POINTER_SIZE = 4;
const SLICE_COUNT_SIZE = 4;

function Constant(value, address) {
  return { type: "Const", value, address };
}

function TaggedPointer(tag, pointer, address) {
  return { type: "Pointer", tag, pointer, address };
}

function Tuple(variant, components, address) {
  return { type: "Tuple",  variant, components, address };
}

function ByteArraySlice(parent_pointer, pointer, count, contents, address) {
  return { type: "ByteArraySlice", parent_pointer, pointer, count, contents, address };
}

// TODO: May be useful during GC inspection.
function Moved(pointer) {
  return { type: "Moved", pointer };
}

function ByteArray(byte_count, bytes, address) {
  return { type: "ByteArray",  byte_count, bytes, address };
}

function Env(old_env_pointer, values, address) {
  return { type: "Env", old_env_pointer, values, address };
}

export function readRawPointer(view, raw_pointer) {
  const tag = view.getInt8(raw_pointer);
  if (tag == CONST_TAG) {
    const value = view.getInt32(raw_pointer + 1, true);
    return Constant(value, raw_pointer);
  } else if (tag == TUPLE_TAG) {
    const tuple_pointer = view.getInt32(raw_pointer + 1, true);
    return TaggedPointer("Tuple", tuple_pointer, raw_pointer);
  } else if (tag == BYTE_ARRAY_SLICE_TAG) {
    const slice_pointer = view.getInt32(raw_pointer + 1, true);
    return TaggedPointer("ByteArraySlice", slice_pointer, raw_pointer);
    throw Error("Slices not yet implemented");
  } else if (tag == BYTE_ARRAY_TAG) {
    throw Error("Arrays not yet implemented");
  } else {
    throw Error(`Unknown value Tag ${tag}`);
  }
}

export function readTuple(view, tagged_pointer) {
  if (tagged_pointer.tag != "Tuple") { throw Error(`Attempt to read non-tuple as tuple @ ${tagged_pointer}`) }
  const tuple_pointer = tagged_pointer.pointer;
  // 1 byte for GC, 1 byte for tag, 4 bytes for variant, 1 byte for count, followed by components
  const variant_offset = 2;
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

export function readSlice(view, tagged_pointer) {
  if (tagged_pointer.tag != "ByteArraySlice") { throw Error(`Attempt to read non-slice as a slice @ ${tagged_pointer}`) }
  const slice_pointer = tagged_pointer.pointer;

  const parent_pointer = view.getInt32(slice_pointer + SLICE_PARENT_POINTER_OFFSET, true);
  const pointer = view.getInt32(slice_pointer + SLICE_POINTER_OFFSET, true);
  const count = view.getInt32(slice_pointer + SLICE_COUNT_OFFSET, true);

  return ByteArraySlice(parent_pointer, pointer, count, undefined, slice_pointer);
}

// WARNING: Does not detect cyclic structure.
export function deepReadRawPointer(view, raw_pointer) {
  const tagged_pointer = readRawPointer(view, raw_pointer);
  return deepReadTaggedPointer(view, tagged_pointer);
}

export function deepReadTaggedPointer(view, tagged_pointer) {
  if (tagged_pointer.type == "Const") {
    return tagged_pointer;
  } else if (tagged_pointer.type == "Pointer"){
    if (tagged_pointer.tag == "Tuple") {
      return deepReadTuple(view, tagged_pointer);
    } else if (tagged_pointer.tag == "ByteArraySlice"){
      return deepReadSlice(view, tagged_pointer);
    } else {
      throw Error(`We have a pointer that's not a tuple ${tagged_pointer}`);
    }
  } else {
    throw Error(`We have a tagged pointer that's neither a Const nor a Pointer ${tagged_pointer}`);
  }
}

export function deepReadTuple(view, tagged_pointer) {
  const tuple = readTuple(view, tagged_pointer);
  const components = [];
  tuple.components.forEach(component => {
    components.push(deepReadTaggedPointer(view, component));
  });
  return Tuple(tuple.variant, components, tuple.address);
}

function deepReadSlice(view, tagged_pointer) {
  const slice = readSlice(view, tagged_pointer);
  const str_view = new DataView(view.buffer, slice.pointer, slice.count)
  const str = (new TextDecoder()).decode(str_view)
  return ByteArraySlice(slice.parent_pointer, slice.pointer, slice.count, str, slice.address);
}

export function readStack(view, raw_pointer_start, raw_pointer_end) {
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
      } else if (tag == BYTE_ARRAY_SLICE_TAG) {
        const slice_pointer = view.getInt32(raw_pointer + 1, true);
        stack.push(TaggedPointer("ByteArraySlice", slice_pointer, raw_pointer));
        readNextValue(raw_pointer + TAGGED_POINTER_BYTE_SIZE)
      } else {
        throw Error(`Unknown value Tag ${tag}`);
      }
    }
  }

  readNextValue(raw_pointer_start);
  return stack;
}

export function deepReadStack(view, raw_pointer_start, raw_pointer_end) {
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
        readNextValue(raw_pointer + TAGGED_POINTER_BYTE_SIZE);
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
      } else if (tag == BYTE_ARRAY_SLICE_TAG) {
        const slice_pointer = view.getInt32(raw_pointer + 1, true);
        const tagged_pointer = TaggedPointer("ByteArraySlice", slice_pointer);
        stack.push(deepReadSlice(view, tagged_pointer));
        readNextValue(raw_pointer + TAGGED_POINTER_BYTE_SIZE);
      } else {
        throw Error(`Unknown value Tag ${tag}`);
      }
    }
  }

  readNextValue(raw_pointer_start);
  return stack;
}

export function showValue(x) {
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
  } else if (x.type == "ByteArraySlice") {
    return `Slice[parent:=${x.parent_pointer}, p:=${x.pointer}, c:=${x.count}]("${x.contents}")`;
  } else if (x.type == "ByteArray") {
    throw Error("Arrays not yet implemented");
  } else {
    throw Error(`Unexpected value type in ${x}`);
  }
}

export function showValueWithAddress(x) {
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
  } else if (x.type == "ByteArraySlice") {
    return `Slice@${x.address}[parent:=${x.parent_pointer}, p:=${x.pointer}, c:=${x.count}]("${x.contents}")`;
  } else if (x.type == "ByteArray") {
    throw Error("Arrays not yet implemented");
  } else {
    throw Error(`Unexpected value type in ${x}`);
  }
}

export function showStack(stack) {
  const strings = [];
  stack.forEach(x => {
    strings.push(showValue(x));
  });

  return `Nil <: ${strings.join(' <: ')}`;
}

export function showStackWithAddress(stack) {
  const strings = [];
  stack.forEach(x => {
    strings.push(showValueWithAddress(x));
  });

  return `Nil <: ${strings.join(' <: ')}`;
}
