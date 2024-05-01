// This module requires: `wasm-merge` and `wat2wasm`

const fs = require("fs");
const child_process = require("child_process");

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

function wat2wasm(wat_path, wasm_path, module_name) {
  const cmd = `wat2wasm ${wat_path}/${module_name}.wat -o ${wasm_path}/${module_name}.wasm`;
  return exec(cmd);
}
module.exports.wat2wasm = wat2wasm;

function wasm_merge(wasm_path, modules, out_module) {
  const merge_args = [];
  modules.forEach(module => {
    merge_args.push(`${wasm_path}/${module}.wasm ${module}`);
  });
  const cmd = `wasm-merge --enable-bulk-memory ${merge_args.join(" ")} -o ${wasm_path}/${out_module}.wasm`;
  return exec(cmd);
}
module.exports.wasm_merge = wasm_merge;

function wasm2bytes(wasm_path, module) {
  const file = fs.readFileSync(`${wasm_path}/${module}.wasm`);
  const bytes = new Uint8Array(file);
  return bytes;
}
module.exports.wasm2bytes = wasm2bytes;

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

    on_instance(instance, { GLOBAL, LOG, buffer, config });
  });
}
module.exports.run = run;
