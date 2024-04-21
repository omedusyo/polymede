const fs = require("fs");

// file is an array of bytes.
const file_name = process.argv[2];
console.log(`file: ${file_name}`);
const file = fs.readFileSync(`./${file_name}`);
const bytes = new Uint8Array(file);


// console.log(bytes);

WebAssembly.instantiate(bytes).then(({ instance }) => {
  const f = instance.exports.f;

  console.log(f(10));
})

