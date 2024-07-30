const fs = require('node:fs');
const { showValue, showValueWithAddress, showStack, showStackWithAddress, deepReadRawPointer, readRawPointer, readTuple, readStack, deepReadStack } = require("./memory_inspect.js");
const { perform } = require("./perform_command.js");

function run(bytes) {
  const stack_pages = 16;
  const heap_pages = 256;
  const total_pages = stack_pages + 2*heap_pages;

  const page_byte_size = 2**16; // 65 KB
  const stack_byte_size = page_byte_size * stack_pages; // 1 MB. Note that this is Linear Stack in Linear Memory, not the wasm's stack.
  const heap_byte_size = page_byte_size * heap_pages; // 16 MB
  const total_byte_size = page_byte_size * total_pages; // (1 + 2*16) MB == 33 MB

  function log_meta() {
    console.log('page_byte_size =', page_byte_size);
    console.log('stack_byte_size =', stack_byte_size);
    console.log('heap_byte_size =', heap_byte_size);
    console.log('total_byte_size =', total_byte_size);
  }
  log_meta();

  const memory = new WebAssembly.Memory({ initial: total_pages, maximum: total_pages }); // 37 MB
  const STACK_SIZE = new WebAssembly.Global({ value: "i32", mutable: false }, stack_byte_size);
  const HEAP_SIZE  = new WebAssembly.Global({ value: "i32", mutable: false }, heap_byte_size);
  const STACK_START  = new WebAssembly.Global({ value: "i32", mutable: false }, 0)
  const A_HEAP_START = new WebAssembly.Global({ value: "i32", mutable: true }, stack_byte_size);
  const B_HEAP_START = new WebAssembly.Global({ value: "i32", mutable: true }, stack_byte_size + heap_byte_size);

  const FREE = new WebAssembly.Global({ value: "i32", mutable: true }, A_HEAP_START.valueOf());


  const GLOBAL = {};
  const view = new DataView(memory.buffer);

  function log_stack(s) {
    console.log(s, showStackWithAddress(deepReadStack(view, GLOBAL.STACK_START.valueOf(), GLOBAL.STACK.valueOf())));
  }

  function log_heap_meta() {
    console.log(`A_HEAP = ${GLOBAL.A_HEAP_START.valueOf()}, B_HEAP = ${GLOBAL.B_HEAP_START.valueOf()}`);
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

      on_stack_overflow: () => {
        throw Error("Linear Stack Overflow!");
      },
      on_garbage_collection: () => {
        console.log("========Performing GC===========");
      },
      on_out_of_memory: () => {
        throw Error("Out of heap memory!");
      },
      debug_int: (x) => {
        console.log("DEBUGGING x =", x);
      },
      debug: () => {
        console.log("DEBUGGING");
      },
      show_stack: (x) => {
        log_stack(x);
      },
    },
    console: {
      log_int(x) {
        console.log(x);
      },
      log_two_ints(x, y) {
        console.log("Logging two ints", x, y);
      },
    },
  };

  WebAssembly.instantiate(bytes, config).then(({ instance }) => {
    const { main, stack, env, free, function_table } = instance.exports;
    const { make_env, make_env_from, make_env_from_closure, drop_env, tuple, partial_apply, get_tuple_pointer, perform_primitive_command, unpack_in_reverse, copy_value_to_stack } = instance.exports;
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

    make_env(0);
    main();
    drop_env();

    console.log("> Main executed succesfully.");
    // log_stack(0);
    console.log("> Performing command...");

    perform(
      view,
      { get_tuple_pointer, perform_primitive_command, unpack_in_reverse, copy_value_to_stack, make_env_from_closure, drop_env  },
      GLOBAL.TABLE,
      true, // tracing
      GLOBAL.STACK_START,
      GLOBAL.STACK,
    );
    // log_stack(1);
    console.log("> Exiting.");

  });
}

function main() {
  const wasm_path = process.argv[2];
  const wasm_buffer = fs.readFileSync(wasm_path);

  run(wasm_buffer);
}

main();
