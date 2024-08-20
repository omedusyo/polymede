const TAGGED_POINTER_BYTE_SIZE = 5;

const variant_offset = 2;
const count_offset = variant_offset + 4;
const components_offset = count_offset + 1;

export function execute_top_continuation(continuation_state, { make_env_from_closure }, TABLE) {
  // stack = [..., k, value]
  const fn_pointer = make_env_from_closure(1);
  TABLE.get(fn_pointer)();
  continuation_state.number_of_continuations_left -= 1;
  // stack = [..., new_command]
}

export function perform(continuation_state, view, operators, TABLE, is_tracing_enabled, STACK_START, STACK) {

  const { get_tuple_pointer, perform_primitive_command, unpack_in_reverse, copy_value_to_stack, make_env_from_closure } = operators;
  let raw_pointer;

  while (true) {
    // stack = [..., command]
    raw_pointer = get_tuple_pointer();
    // stack = [...,]
    const op_code = view.getInt32(raw_pointer + variant_offset, true);
    if (op_code == 0) {
      // ===pure===
      copy_value_to_stack(raw_pointer + components_offset);
      // stack = [..., value]
      if (continuation_state.number_of_continuations_left == 0) {
        break;
      } else {
        execute_top_continuation(continuation_state, operators, TABLE);
      }
    } else if (op_code == 1) {
      // ===and_then===
      copy_value_to_stack(raw_pointer + components_offset);
      // push continuation to the stack
      continuation_state.number_of_continuations_left += 1;
      // pushes the inner command to the stack
      copy_value_to_stack(raw_pointer + components_offset + TAGGED_POINTER_BYTE_SIZE);
    } else if (op_code == 2) {
      // ===receive===
      // parking continuation
      break;
    } else {
      // ===primitive operation===
      if (is_tracing_enabled) { console.log(`TRACE: OP(${op_code})`); }
      const arg_count = view.getInt8(raw_pointer + count_offset, true);
      unpack_in_reverse(raw_pointer + components_offset, arg_count);
      // Note that `perform_primitive_command` is responsible for managing the return value on the stack afterwards.
      perform_primitive_command(op_code);
      if (continuation_state.number_of_continuations_left == 0) {
        break;
      } else {
        execute_top_continuation(continuation_state, operators, TABLE);
      }
    }
  }
}
