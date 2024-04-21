const fs = require("fs");

// file is an array of bytes.
const file_name = process.argv[2];
console.log(`file: ${file_name}`);
const file = fs.readFileSync(`./${file_name}`);
const bytes = new Uint8Array(file);
// console.log(bytes);

function printRawStack(buffer, stack_start, stack) {
  const array = new Uint8Array(buffer);
  const start = stack_start.valueOf();
  const end = stack.valueOf();
  console.log(`STACK[${start} to ${end}]`, array.slice(start, end));
}

function printStack(buffer, stack_start, stack) {
  // TODO
}

function printRawHeap(buffer, heap, free) {
  const array = new Uint8Array(buffer);
  const start = heap.valueOf();
  const end = free.valueOf();
  console.log(`HEAP[${start} to ${end}]`, array.slice(start, end));
}

function printHeap(buffer, heap, free) {
  // TODO: This probably doesn't even make sense
}


let GLOBAL_BUFFER;
let GLOBAL_STACK_START;
let GLOBAL_STACK;
const config = {
  console: {
    log(x) {
      console.log(x);
    },
    logStack() {
      printRawStack(GLOBAL_BUFFER, GLOBAL_STACK_START, GLOBAL_STACK);
    },
    logHeap() {
      printRawHeap(GLOBAL_BUFFER, GLOBAL_HEAP, GLOBAL_FREE);
    },
  },
};

WebAssembly.instantiate(bytes, config).then(({ instance }) => {
  const { memory, init, stack_start, stack, heap, free} = instance.exports;

  GLOBAL_BUFFER = memory.buffer;
  GLOBAL_STACK_START = stack_start;
  GLOBAL_STACK = stack;
  GLOBAL_HEAP = heap;
  GLOBAL_FREE = free;

  const buffer = memory.buffer;
  const array = new Uint8Array(buffer);

  init()
});

