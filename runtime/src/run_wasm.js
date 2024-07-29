const fs = require('node:fs');
const { showValue, showValueWithAddress, showStack, showStackWithAddress, deepReadRawPointer, readRawPointer, readTuple, readStack, deepReadStack } = require("./memory_inspect.js");
const { perform } = require("./perform_command.js");

function run(bytes) {
  const stack_pages = 16;
  // TODO: Once GC is implemented, don't forget to increase the memory
  // const heap_pages = 256;
  const heap_pages = 1;
  const total_pages = stack_pages + 2*heap_pages;

  const page_byte_size = 2**16; // 65 KB
  const stack_byte_size = page_byte_size * stack_pages; // 1 MB. Note that this is Linear Stack in Linear Memory, not the wasm's stack.
  const heap_byte_size = page_byte_size * heap_pages; // 16 MB
  const total_byte_size = page_byte_size * total_pages; // (1 + 2*16) MB == 33 MB

  const memory = new WebAssembly.Memory({ initial: total_pages, maximum: total_pages }); // 37 MB

  const config = {
    env: {
      memory,
      stack_size: new WebAssembly.Global({ value: "i32", mutable: false }, stack_byte_size),
      heap_size: new WebAssembly.Global({ value: "i32", mutable: false }, heap_byte_size),
      on_stack_overflow: () => {
        throw Error("Linear Stack Overflow!");
      },
      on_garbage_collection: () => {
        console.log("========Performing GC===========");
      },
      on_out_of_memory: () => {
        throw Error("Out of heap memory!");
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

  const GLOBAL = {};

  WebAssembly.instantiate(bytes, config).then(({ instance }) => {
    const { main, init, stack_start, stack, env, heap, free, function_table } = instance.exports;
    const { make_env, make_env_from, make_env_from_closure, drop_env, tuple, partial_apply, get_tuple_pointer, perform_primitive_command, unpack_in_reverse, copy_value_to_stack } = instance.exports;

    
    const const_ = instance.exports["const"];
    const var_ = instance.exports["var"];

    GLOBAL.STACK_START = stack_start;
    GLOBAL.STACK = stack;
    GLOBAL.ENV = env;
    GLOBAL.HEAP = heap;
    GLOBAL.FREE = free;
    GLOBAL.TABLE = function_table;

    function log_stack(s) {
      console.log(s, showStackWithAddress(deepReadStack(view, GLOBAL.STACK_START.valueOf(), GLOBAL.STACK.valueOf())));
    }

    console.log("> Instantiated succesfully.");

    // console.log(main);
    const view = new DataView(memory.buffer);

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
