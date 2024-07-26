const fs = require('node:fs');
const { showValue, deepReadRawPointer, readRawPointer, readTuple } = require("./memory_inspect.js");

function run(bytes) {
  const memory = new WebAssembly.Memory({ initial: 2, maximum: 10 });
  const closure_table = new WebAssembly.Table({ initial: 0, element: "anyfunc" });

  const config = {
    env: {
      memory,
      closure_table,
    },
  };

  const GLOBAL = {};

  function printRawStack() {
    const array = new Uint8Array(memory.buffer);
    const start = GLOBAL.STACK_START.valueOf();
    const end = GLOBAL.STACK.valueOf();


    //=====
    const view = new DataView(memory.buffer);
    // let tagged_pointer = readRawPointer(view, start);
    // console.log(tagged_pointer);
    // let tuple0 = readTuple(view, tagged_pointer);
    // console.log(tuple0);
    // let tuple1 = readTuple(view, tuple0.components[1]);
    // console.log(tuple1);

    console.log("+===================+");
    const val = deepReadRawPointer(view, start);
    console.log(showValue(val))

    //=====

    const values = array.slice(start, end);
    // values.forEach(v => {
    //   console.log(v);
    // });
    // console.log(`STACK[${start} to ${end}]`, values);
  }

  WebAssembly.instantiate(bytes, config).then(({ instance }) => {
    const { main, init, stack_start, stack, env, heap, free, frame } = instance.exports;

    GLOBAL.STACK_START = stack_start;
    GLOBAL.STACK = stack;
    GLOBAL.ENV = env;
    GLOBAL.HEAP = heap;
    GLOBAL.FREE = free;
    GLOBAL.FRAME = frame;

    console.log("Instantiated succesfully");
    console.log(main);
    console.log(main());

    printRawStack();
  });
}

function main() {
  const wasm_path = process.argv[2];
  const wasm_buffer = fs.readFileSync(wasm_path);
  console.log(wasm_buffer);

  run(wasm_buffer);
}

main();

