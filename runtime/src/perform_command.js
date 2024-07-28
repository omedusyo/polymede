const { showValue, showValueWithAddress, showStack, showStackWithAddress, deepReadRawPointer, readRawPointer, readTuple, readStack, deepReadStack } = require("./memory_inspect.js");

const TAGGED_POINTER_BYTE_SIZE = 5;

const variant_offset = 1;
const count_offset = variant_offset + 4;
const components_offset = count_offset + 1;

function perform(view, operators, TABLE, is_tracing_enabled, trace) {
  // TODO: Add a flag for tracinga.

  const { get_tuple_pointer, perform_primitive_command, unpack_in_reverse, copy_value_to_stack, make_env_from_closure, drop_env } = operators;
  let raw_pointer 
  let number_of_continuations_left = 0; // Counts number of continuations to be done on the stack.

  function execute_top_continuation() {
    // stack = [..., k, value]
    const fn_pointer = make_env_from_closure(1);
    TABLE.get(fn_pointer)();
    drop_env();
    number_of_continuations_left -= 1;
    // stack = [..., new_command]
  }

  while (true) {
    // stack = [..., command]
    raw_pointer = get_tuple_pointer();
    // stack = [...,]
    const op_code = view.getInt32(raw_pointer + variant_offset, true);
    is_tracing_enabled
    if (op_code == 0) {
      if (is_tracing_enabled) { console.log(`TRACE: Pure`); }
      // ===pure===
      copy_value_to_stack(raw_pointer + components_offset);
      if (number_of_continuations_left == 0) {
        break;
      } else {
        execute_top_continuation()
      }
    } else if (op_code == 1) {
      if (is_tracing_enabled) { console.log(`TRACE: AND_THEN`); }
      // ===and_then===
      // push continuation to the stack
      copy_value_to_stack(raw_pointer + components_offset + TAGGED_POINTER_BYTE_SIZE);
      number_of_continuations_left += 1;
      // pushes the inner command to the stack
      copy_value_to_stack(raw_pointer + components_offset);
    } else {
      // ===primitive operation===
      if (is_tracing_enabled) { console.log(`TRACE: OP(${op_code})`); }
      const arg_count = view.getInt8(raw_pointer + count_offset, true);
      unpack_in_reverse(raw_pointer + components_offset, arg_count);
      // Note that `perform_primitive_command` is responsible for managing the return value on the stack afterwards.
      perform_primitive_command(op_code);
      if (number_of_continuations_left == 0) {
        break;
      } else {
        execute_top_continuation();
      }
    }
  }
}
module.exports.perform = perform;
