import { execute_top_continuation, perform } from "./perform_command.js";
import { Value, Tag } from "./message.js";
import { showValue, showValueWithAddress, showStack, showStackWithAddress, deepReadRawPointer, readRawPointer, readTuple, readStack, deepReadStack } from "./memory_inspect.js";

// ArrayBuffer -> Promise<PolymedeActor>
export function spawn(bytes) {

  return WebAssembly.compile(bytes).then(module => {
    const POLYMEDE_STATIC_CUSTOM_SECTION = WebAssembly.Module.customSections(module, "POLYMEDE_STATIC");
    if (POLYMEDE_STATIC_CUSTOM_SECTION.length == 0) {
      throw Error("wasm module doesn't contain POLYMEDE_STATIC custom section. Did you get your order of wasm-merge arguments wrong (it can strip custom sections in the wrong order)?");
    }

    const STATIC_BUFFER = POLYMEDE_STATIC_CUSTOM_SECTION[0];
    const static_byte_size = STATIC_BUFFER.byteLength;

    const page_byte_size = 2**16; // 65 KB
    const static_pages = Math.ceil(static_byte_size / page_byte_size);
    const stack_pages = 16;
    const heap_pages = 256;
    const total_pages = static_pages + stack_pages + 2*heap_pages;

    const stack_byte_size = page_byte_size * stack_pages; // 1 MB. Note that this is Linear Stack in Linear Memory, not the wasm's stack.
    const heap_byte_size = page_byte_size * heap_pages; // 16 MB
    const total_used_byte_size = static_byte_size + stack_byte_size + 2*heap_byte_size;
    const total_allocated_byte_size = page_byte_size * total_pages; // (1 + 2*16) MB == 33 MB (not counting STATIC)

    const memory = new WebAssembly.Memory({ initial: total_pages, maximum: total_pages }); // 37 MB (not counting STATIC)
    const STACK_SIZE = new WebAssembly.Global({ value: "i32", mutable: false }, stack_byte_size);
    const HEAP_SIZE  = new WebAssembly.Global({ value: "i32", mutable: false }, heap_byte_size);
    const STATIC_START = new WebAssembly.Global({ value: "i32", mutable: false }, 0);
    const STACK_START  = new WebAssembly.Global({ value: "i32", mutable: false }, static_byte_size)
    const A_HEAP_START = new WebAssembly.Global({ value: "i32", mutable: true }, static_byte_size + stack_byte_size);
    const B_HEAP_START = new WebAssembly.Global({ value: "i32", mutable: true }, static_byte_size + stack_byte_size + heap_byte_size);

    const FREE = new WebAssembly.Global({ value: "i32", mutable: true }, A_HEAP_START.valueOf());

    const GLOBAL = {};

    function log_meta() {
      console.log('static_byte_size =', static_byte_size);
      console.log('page_byte_size =', page_byte_size);
      console.log('stack_byte_size =', stack_byte_size);
      console.log('heap_byte_size =', heap_byte_size);
      console.log('total_used_byte_size =', total_used_byte_size);
      console.log(`total_allocated_byte_size = ${total_allocated_byte_size} = pages * page_size = ${total_pages} * ${page_byte_size}`);
    }
    log_meta();

    const view = new DataView(memory.buffer);

    // ===Copy the STATIC region to Linear Memory===
    const static_view = new DataView(STATIC_BUFFER);
    for (let i = 0; i < static_byte_size; i++) {
      // TODO: There must be a much better way of copying one buffer into another.
      view.setUint8(i, static_view.getUint8(i));
    }

    function log_heap_meta() {
      console.log(`A_HEAP = ${GLOBAL.A_HEAP_START.valueOf()}, B_HEAP = ${GLOBAL.B_HEAP_START.valueOf()}`);
    }

    // ===Canvas===
    // const canvas_el = document.querySelector("#canvas");
    // if (!canvas_el.getContext) { throw Error("No canvas detected!"); }
    //
    // const ctx = canvas_el.getContext("2d");
    const ctx = { }; // fake canvas for command-line

    function log_stack(s) {
      console.log(s, showStackWithAddress(deepReadStack(view, GLOBAL.STACK_START.valueOf(), GLOBAL.STACK.valueOf())));
    }

    const config = {
      env: {
        memory,
        STACK_SIZE,
        HEAP_SIZE,
        STACK_START,
        A_HEAP_START,
        B_HEAP_START,
        FREE,
        STATIC_START,

        on_stack_overflow: () => { throw Error("Linear Stack Overflow!"); },
        on_garbage_collection: () => { console.log("========Performing GC==========="); },
        on_out_of_memory: () => { throw Error("Out of heap memory!"); },
        debug_int: (x) => { console.log("DEBUGGING x =", x); },
        debug: () => { console.log("DEBUGGING"); },
        show_stack: (x) => {
          log_stack(x);
        },
      },
      console: {
        log_int(x) { console.log(x); },
        log_f32(x) { console.log(x); },
        log_two_ints(x, y) { console.log("Logging two ints", x, y); },
        log_string(pointer, byte_count) {
          const string_view = new DataView(memory.buffer, pointer,byte_count);
          let utf8decoder = new TextDecoder();
          let s = utf8decoder.decode(string_view);
          console.log(s);
        }
      },
      canvas: {
        get_width() { return ctx.canvas.clientWidth; },
        get_height() { return ctx.canvas.clientHeight; },
        fill(r, g, b, a) {
          ctx.fillStyle = `rgba(${r}, ${g}, ${b}, ${a})`;
        },
        stroke(r, g, b, a) {
          ctx.strokeStyle = `rgba(${r}, ${g}, ${b}, ${a})`;
        },
        line_width(x) {
          ctx.lineWidth = x;
        },
        fill_rect(x, y, w, h) {
          ctx.fillRect(x, y, w, h);
        },
      }
    };


    return WebAssembly.instantiate(module, config).then(instance => {
        const { main, stack, env, free, function_table, STATIC_SIZE } = instance.exports;
        const { make_env, make_env_from, make_env_from_closure, drop_env, array_slice, tuple, float32, partial_apply, get_tuple_pointer, perform_primitive_command, unpack_in_reverse, copy_value_to_stack, gc_if_out_of_space, inc_free } = instance.exports;
        const { gc, gc_move_tuple, gc_traverse_grey, gc_walk_stack } = instance.exports;

        const const_ = instance.exports["const"];
        const var_ = instance.exports["var"];

        GLOBAL.STACK_START = config.env.STACK_START;
        GLOBAL.A_HEAP_START = config.env.A_HEAP_START;
        GLOBAL.B_HEAP_START = config.env.B_HEAP_START;
        GLOBAL.STACK = stack;
        GLOBAL.ENV = env;
        GLOBAL.FREE = free;
        GLOBAL.TABLE = function_table;

        console.log("> Instantiated succesfully.");
        log_heap_meta()

        main();

        console.log("> Main executed succesfully.");
        log_stack(0);
        console.log("> Performing command...");

        function make_string(str) {
          // TODO: move
          const BYTE_ARRAY_HEADER_BYTE_SIZE = 6;
          const BYTE_ARRAY_CONTENTS_OFFSET = 6;
          const GC_TAG_LIVE = 0;
          const GC_TAG_BYTE_SIZE = 1;
          const BYTE_ARRAY_TAG = 3;
          const TAG_BYTE_SIZE = 1;
          const BYTE_ARRAY_COUNT_SIZE = 4;

          const encoded_str = new TextEncoder().encode(str);
          gc_if_out_of_space(BYTE_ARRAY_HEADER_BYTE_SIZE + encoded_str.length);
          const string_pointer = FREE.valueOf();

          // ===create header===
          // 1 byte for garbage collection liveness.
          view.setUint8(FREE.valueOf(), GC_TAG_LIVE);
          inc_free(GC_TAG_BYTE_SIZE);
          // 1 byte tag (used by gc going from grey ~> black)
          view.setUint8(FREE.valueOf(), BYTE_ARRAY_TAG);
          inc_free(TAG_BYTE_SIZE);

          // 4 byte count 
          view.setUint32(FREE, encoded_str.length, true);
          inc_free(BYTE_ARRAY_COUNT_SIZE);

          // ===copy string
          let raw_pointer = FREE.valueOf();
          for (let i = 0; i < encoded_str.length; i++) {
            view.setUint8(raw_pointer, encoded_str[i]);
            raw_pointer += 1;
          }
          inc_free(encoded_str.length)

          // ===create slice===
          array_slice(string_pointer, BYTE_ARRAY_CONTENTS_OFFSET + string_pointer, encoded_str.length);
        }
        
        const continuation_state = {
          number_of_continuations_left: 0, // Counts number of continuations to be done on the stack.
        };

        function p() {
          perform(
            continuation_state,
            view,
            { get_tuple_pointer, perform_primitive_command, unpack_in_reverse, copy_value_to_stack, make_env_from_closure  },
            GLOBAL.TABLE,
            false, // tracing
            GLOBAL.STACK_START,
            GLOBAL.STACK,
          );
        }
        p();

        console.log("> Exiting.");

        return {
          // ========Send Message==========
          send(msg) {
            function push_msg(msg) {
              switch (msg.tag) {
                case Tag.Constant:
                  const_(msg.value);
                  break;
                case Tag.Float:
                  float32(msg.value);
                  break;
                case Tag.String:
                  make_string(msg.value);
                  break;
                case Tag.Tuple:
                  for (let i = 0; i < msg.args.length; i++) {
                    push_msg(msg.args[i]);
                  }
                  tuple(msg.variant, msg.args.length);
                  break;
                default:
                  throw Error(`Attempt to send a malformed message: ${msg}`)
              }
            }
            push_msg(msg);
            // resume continuation
            if (continuation_state.number_of_continuations_left == 0) {
              return
            } else {
              // stack = [..., k, msg]
              // TODO: This can cause stack overflow, right?
              execute_top_continuation(continuation_state, { make_env_from_closure }, GLOBAL.TABLE);
              p();
            }
          }
        };
    });
  });
}
