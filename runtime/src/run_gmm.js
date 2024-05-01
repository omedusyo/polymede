const { wat2wasm, wasm_merge, wasm2bytes, run } = require("./wasm_helpers.js");

function main() {
  const module_name = process.argv[2];
  console.log(`module_name: ${module_name}`);

  compile(module_name).then(bytes => {
    console.log("Compiled.")

    run(bytes, ({ exports }, { LOG, config }) => {
      console.log("Running module...")
      exports.main();
      config.console.logStack();
    });


  }).catch(err => {
    console.log(err);
  });
}

function compile(in_module) {
  const runtime_module = "runtime";
  const output_module = "output";

  const wat_path = "runtime/src/wasm";
  const wasm_path = "tmp_wasm";

  // ===.wat compilation===
  return wat2wasm(wat_path, wasm_path, runtime_module)
    // Note that we've skipped wat2wasm compilation of the in_module
    // ===static merging===
    .then(_ => wasm_merge(wasm_path, [in_module, runtime_module], output_module))
    .then(_ => {
      return wasm2bytes(wasm_path, output_module);
    });
}

main();
