const path = require('path');

module.exports = {
  entry: './{ENTRY}' ,
  context: path.resolve(__dirname, '.'),
  output: {
    path: path.resolve(__dirname, "."),
    hashFunction: "xxhash64",
  },
  mode : 'production',
  stats: 'errors-only',
};
