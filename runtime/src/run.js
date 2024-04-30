// Requires: `wasm-merge` and `wat2wasm`

const fs = require("fs");
// const exec = require("child_process").exec;
const child_process = require("child_process");
const assert = require('assert');

function exec(cmd) {
  return new Promise((resolve, reject) => {
    child_process.exec(cmd, (err, stdout, stderr) => {
      if (err) {
        reject(stderr);
      } else {
        resolve(stdout);
      }
    });
  });
}

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

TESTS.push(({ exports }, { LOG }) => {
  console.log("RANGE TEST 0");
  exports.range_test_0(5);
  assert.deepEqual(LOG,
    [0, 1, 2, 3, 4, 5555555]
  );
});


// ===Setup===
function main() {
  const module_name = process.argv[2];
  console.log(`module_name: ${module_name}`);

  compile(module_name).then(bytes => {
    // run tests
    TESTS.forEach(test => {
      run(bytes, test);
    });
  }).catch(err => {
    console.log(err);
  });
}

function compile(in_module) {
  // Compiles runtime + module and links them into one `output.wasm` module

  const runtime_module = "runtime";
  const output_module = "output";

  const wat_path = "runtime/src/wasm";
  const wasm_path = "tmp_wasm/runtime";

  // ===.wat compilation===
  return wat2wasm(wat_path, wasm_path, in_module)
    .then(_ => wat2wasm(wat_path, wasm_path, runtime_module))
  // ===static merging===
    .then(_ => wasm_merge(wasm_path, [in_module, runtime_module], output_module))
  // ===bytes===
    .then(_ => {
      return wasm2bytes(wasm_path, output_module);
    });
}

function wat2wasm(wat_path, wasm_path, module_name) {
  const cmd = `wat2wasm ${wat_path}/${module_name}.wat -o ${wasm_path}/${module_name}.wasm`;
  return exec(cmd);
}

function wasm_merge(wasm_path, modules, out_module) {
  const merge_args = [];
  modules.forEach(module => {
    merge_args.push(`${wasm_path}/${module}.wasm ${module}`);
  });
  const cmd = `wasm-merge --enable-bulk-memory ${merge_args.join(" ")} -o ${wasm_path}/${out_module}.wasm`;
  return exec(cmd);
}

function wasm2bytes(wasm_path, module) {
  const file = fs.readFileSync(`${wasm_path}/${module}.wasm`);
  const bytes = new Uint8Array(file);
  return bytes;
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
