const fs = require("fs");
const exec = require("child_process").exec;
const assert = require('assert');

const TESTS = [];

// ===Tests===
TESTS.push(({ exports }, { LOG }) => {
  console.log("EXAMPLE STACK 0");
  exports.example_stack_0();
  assert.deepEqual(LOG,
    [ 1,    5,0,0,0, // 5
      1,    7,0,0,0, // 7

      1,   12,0,0,0, // 5 + 7 == 12
    ]
  );
});

// TESTS.push(({ exports }, { LOG }) => {
//   console.log("RANGE TEST 0-");
//   exports.range_test_0();
//   console.log("LOG IS ", LOG);
// });


// ===Setup===
function main() {
  const module_name = process.argv[2];
  console.log(`module_name: ${module_name}`);

  wat2wasm(module_name, (bytes) => {
    // run tests
    TESTS.forEach(test => {
      run(bytes, test);
    });
  });
}

function wat2wasm(module_name, on_success) {
  const wasm_out = `tmp_wasm/runtime/${module_name}.wasm`;
  const cmd = `wat2wasm runtime/src/wasm/${module_name}.wat -o ${wasm_out}`;
  exec(cmd, (err, stdout, stderr) => {
    if (err) {
      console.log("ERROR", stderr);
    } else {
      const file = fs.readFileSync(wasm_out);
      const bytes = new Uint8Array(file);
      on_success(bytes);
    }
  });
}

function run(bytes, on_instance) {

  function printRawStack(buffer) {
    const array = new Uint8Array(buffer);
    const start = GLOBAL.STACK_START.valueOf();
    const end = GLOBAL.STACK.valueOf();

    const values = array.slice(start, end);
    values.forEach(v => {
      LOG.push(v);
    });
    console.log(`STACK[${start} to ${end}]`, values);
  }

  function showGlobals() {
    console.log(`STACK=${GLOBAL.STACK.valueOf()}, ENV=${GLOBAL.ENV.valueOf()}, FREE=${GLOBAL.FREE.valueOf()}`);
  }

  function printStack(buffer) {
    // TODO
  }

  function printRawHeap(buffer) {
    const array = new Uint8Array(buffer);
    const start = GLOBAL.HEAP.valueOf();
    const end = GLOBAL.FREE.valueOf();
    console.log(`HEAP[${start} to ${end}]`, array.slice(start, end));
  }

  function printHeap(buffer, heap, free) {
    // TODO: This probably doesn't even make sense
  }

  const GLOBAL = {};
  const LOG = [];
  let buffer;

  const config = {
    console: {
      log(x) {
        LOG.push(x);
        console.log(x);
      },
      logStack() {
        showGlobals();
        printRawStack(buffer);
      },
      logHeap() {
        printRawHeap(buffer);
      },
    },
  };

  WebAssembly.instantiate(bytes, config).then(({ instance }) => {
    const { memory, init, stack_start, stack, env, heap, free, frame } = instance.exports;
    buffer = memory.buffer;

    GLOBAL.BUFFER = memory.buffer;
    GLOBAL.STACK_START = stack_start;
    GLOBAL.STACK = stack;
    GLOBAL.ENV = env;
    GLOBAL.HEAP = heap;
    GLOBAL.FREE = free;
    GLOBAL.FRAME = frame;

    const array = new Uint8Array(buffer);

    on_instance(instance, { GLOBAL, LOG, buffer });
  });
}

main();
