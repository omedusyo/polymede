const fs = require("fs");

// file is an array of bytes.
const file_name = process.argv[2];
console.log(`file: ${file_name}`);
const file = fs.readFileSync(`./${file_name}`);
const bytes = new Uint8Array(file);
// console.log(bytes);

function printRawStack(buffer) {
  const array = new Uint8Array(buffer);
  const start = GLOBAL.STACK_START.valueOf();
  const end = GLOBAL.STACK.valueOf();
  console.log(`STACK[${start} to ${end}]`, array.slice(start, end));
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
let buffer;

const config = {
  console: {
    log(x) {
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

  init()
});

