const fs = require('node:fs');
const { showValue, showValueWithAddress, showStack, showStackWithAddress, deepReadRawPointer, readRawPointer, readTuple, readStack, deepReadStack } = require("./memory_inspect.js");
const { perform } = require("./perform_command.js");

function run(bytes) {
  // TODO: Once GC is implemented, don't forget to increase the memory
  const stack_pages = 16;
  // const heap_pages = 256;
  // TODO: This is for debugging
  // const stack_pages = 1;
  const heap_pages = 1;
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
  // TODO: Revert to the above!
  // TODO: This is for debugging
  // const A_HEAP_START = new WebAssembly.Global({ value: "i32", mutable: true }, 2**16 + 2**14);
  // const B_HEAP_START = new WebAssembly.Global({ value: "i32", mutable: true }, 2**16 + 2**16);

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

    // console.log(main);

    make_env(0);
    main();
    drop_env();

    console.log("> Main executed succesfully.");
    // log_stack("");
    console.log("> Performing command...");

    perform(
      view,
      { get_tuple_pointer, perform_primitive_command, unpack_in_reverse, copy_value_to_stack, make_env_from_closure, drop_env  },
      GLOBAL.TABLE,
      true, // tracing
      GLOBAL.STACK_START,
      GLOBAL.STACK,
    );
    // log_stack("");

    // ============GC Debugging============
    console.log("=======GC Debugging==========")
    log_heap_meta();
    // TODO: Just disable main() and perform()
    //       and create custom stack and perform gc manually.

    // const_(16);
    // const_(32);
    // const_(64);
    // tuple(1234, 2);
    // const_(128);
    // tuple(1235, 2); 
    // log_stack(0);

    // ==manual stack walk==
    // FREE.value = B_HEAP_START.value;
    // console.log(readRawPointer(view, 5));
    // var moved_to0 = gc_move_tuple(81936);
    // console.log("Moved to", moved_to0);
    // // I need to refresh the stack.
    // view.setInt32(6, moved_to0, true);

    // ==auto stack walk==
    // FREE.value = B_HEAP_START.value;
    // gc_walk_stack()



    // Here the 1235 tuple is moved, and is grey (i.e. its first component points to A-space, so we need to move that too)
    // gc();
    // log_heap_meta();
    // log_stack(1);
    // gc();
    // log_heap_meta();
    // log_stack(2);
    // TODO: What is going wrong?

    // const TAGGED_POINTER_BYTE_SIZE = 5;
    //
    // const variant_offset = 1;
    // const count_offset = variant_offset + 4;
    // const components_offset = count_offset + 1;
    // log_stack(0);
    // let raw_pointer = get_tuple_pointer();
    // log_stack(1);
    // copy_value_to_stack(raw_pointer + components_offset);
    // log_stack(2);
    // copy_value_to_stack(raw_pointer + components_offset + TAGGED_POINTER_BYTE_SIZE);
    // log_stack(3);
    // const fn_pointer = make_env_from_closure(1);
    // log_stack(4);
    // GLOBAL.TABLE.get(fn_pointer)();
    // log_stack(5);

    console.log("> Exiting.");

    //=====
    // let tagged_pointer = readRawPointer(view, start);
    // console.log(tagged_pointer);
    // let tuple0 = readTuple(view, tagged_pointer);
    // console.log(tuple0);
    // let tuple1 = readTuple(view, tuple0.components[1]);
    // console.log(tuple1);

    // const val = deepReadRawPointer(view, GLOBAL.STACK_START.valueOf());
    // console.log(showValue(val))

    // const_(16);
    // const_(250);
    // make_env(2)

    // const_(16);
    // const_(250);
    // partial_apply(888, 2)
    //
    // console.log(showStackWithAddress(deepReadStack(view, GLOBAL.STACK_START.valueOf(), GLOBAL.STACK.valueOf())));
    //
    // // console.log("What is this?", readRawPointer(view, 2079));
    //
    // const_(123);
    // const_(124);
    // const_(125);
    // console.log(showStackWithAddress(deepReadStack(view, GLOBAL.STACK_START.valueOf(), GLOBAL.STACK.valueOf())));
    // make_env_from_closure(3)
    // console.log(showStackWithAddress(deepReadStack(view, GLOBAL.STACK_START.valueOf(), GLOBAL.STACK.valueOf())));
    // console.log(showStackWithAddress(deepReadStack(view, GLOBAL.STACK_START.valueOf(), GLOBAL.STACK.valueOf())));
    // console.log(readStack(view, GLOBAL.STACK_START.valueOf(), 5))


    // ===Test===
    // console.log(showStackWithAddress(deepReadStack(view, GLOBAL.STACK_START.valueOf(), GLOBAL.STACK.valueOf())));
    // const_(50)
    // const_(60)
    // make_env(2)
    // var_(1)
    // var_(0)
    // console.log(showStackWithAddress(deepReadStack(view, GLOBAL.STACK_START.valueOf(), GLOBAL.STACK.valueOf())));

    // console.log("STACK_START == ", GLOBAL.STACK_START.valueOf());
    // console.log("STACK == ", GLOBAL.STACK.valueOf());
    // const stack_val = readStack(view, GLOBAL.STACK_START.valueOf(), GLOBAL.STACK.valueOf());
    // const stack_val = deepReadStack(view, GLOBAL.STACK_START.valueOf(), GLOBAL.STACK.valueOf());
    // console.log(showStack(stack_val));
    //=====
  });
}

function main() {
  const wasm_path = process.argv[2];
  const wasm_buffer = fs.readFileSync(wasm_path);

  run(wasm_buffer);
}

main();
