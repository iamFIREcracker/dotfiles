var util = require("util");

global.pr = function (...args) {
  for (let each of args) {
    process.stdout.write(
      util.inspect(each, { showHidden: false, depth: null })
    );
    process.stdout.write(" ");
  }
  process.stdout.write("\n");
  return args[0];
};
