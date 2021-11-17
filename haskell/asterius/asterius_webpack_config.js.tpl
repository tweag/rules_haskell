const path = require('path');

module.exports = {
  entry: './{ENTRY}' ,
  context: path.resolve(__dirname, '.'),
  output: {
    path: path.resolve(__dirname, "."),
  },
  mode : 'production',
  stats: 'errors-only',
};
