const assert = require("assert");
const { wat2wasm, wasm_merge, wasm2bytes, run } = require("./wasm_helpers.js");

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
  const module_name = "test";

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

main();
