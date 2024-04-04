const fs = require("fs");

// file is an array of bytes.
const file_name = process.argv[2];
const fn_name = process.argv[3];
console.log(`file: ${file_name}`);
const file = fs.readFileSync(`./${file_name}`);
const bytes = new Uint8Array(file);

// console.log(bytes);

WebAssembly.instantiate(bytes, {
  console: {
    log(x) {
      console.log(x);
    }
  },
}).then(({ instance }) => {
  const f = instance.exports[fn_name];

  // console.log(f(6));
  console.log(f());
});

