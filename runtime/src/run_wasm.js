const fs = require('node:fs');
const { showValue, showValueWithAddress, showStack, showStackWithAddress, deepReadRawPointer, readRawPointer, readTuple, readStack, deepReadStack } = require("./memory_inspect.js");
const { perform } = require("./perform_command.js");

function run(bytes) {
  const memory = new WebAssembly.Memory({ initial: 2, maximum: 10 });

  const config = {
    env: {
      memory,
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

  function printRawStack() {
    const array = new Uint8Array(memory.buffer);
    const start = GLOBAL.STACK_START.valueOf();
    const end = GLOBAL.STACK.valueOf();


    const values = array.slice(start, end);
    values.forEach(v => {
      console.log(v);
    });
    console.log(`STACK[${start} to ${end}]`, values);
  }

  WebAssembly.instantiate(bytes, config).then(({ instance }) => {
    const { main, init, stack_start, stack, env, heap, free, frame, function_table } = instance.exports;
    const { make_env, make_env_from, make_env_from_closure, drop_env, tuple, partial_apply, get_tuple_pointer, perform_primitive_command, unpack_in_reverse, copy_value_to_stack } = instance.exports;

    
    const const_ = instance.exports["const"];
    const var_ = instance.exports["var"];

    GLOBAL.STACK_START = stack_start;
    GLOBAL.STACK = stack;
    GLOBAL.ENV = env;
    GLOBAL.HEAP = heap;
    GLOBAL.FREE = free;
    GLOBAL.FRAME = frame;
    GLOBAL.TABLE = function_table;

    console.log("> Instantiated succesfully.");

    console.log(main);
    // TODO
    // console.log("MAIN IS NOT RUNNING!");
    const view = new DataView(memory.buffer);
    console.log(showStack(deepReadStack(view, GLOBAL.STACK_START.valueOf(), GLOBAL.STACK.valueOf())));

    make_env(0);
    main();
    drop_env();

    console.log("> Main executed succesfully.");
    console.log(showStackWithAddress(deepReadStack(view, GLOBAL.STACK_START.valueOf(), GLOBAL.STACK.valueOf())));


    console.log("> Performing command...");

    perform(
      view,
      { get_tuple_pointer, perform_primitive_command, unpack_in_reverse, copy_value_to_stack, make_env_from_closure, drop_env  },
      GLOBAL.TABLE,
      true,
      GLOBAL.STACK_START, GLOBAL.STACK,
    );
    console.log(showStackWithAddress(deepReadStack(view, GLOBAL.STACK_START.valueOf(), GLOBAL.STACK.valueOf())));

    console.log("> Exiting.");
    // printRawStack();

    //=====
    // let tagged_pointer = readRawPointer(view, start);
    // console.log(tagged_pointer);
    // let tuple0 = readTuple(view, tagged_pointer);
    // console.log(tuple0);
    // let tuple1 = readTuple(view, tuple0.components[1]);
    // console.log(tuple1);

    console.log("+===================+");
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
  console.log(wasm_buffer);

  run(wasm_buffer);
}

main();

