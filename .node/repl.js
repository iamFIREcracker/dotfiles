var net = require("net");
var { DynamicVariable } = require("dynamic_variables.js");
var { defun, refun } = require("dynamic_functions.js").moduleLocal(module);

var repl = new DynamicVariable();

const server = net.createServer(refun(module, "createREPL"));
server.listen(5001, "localhost");

defun(module, function createREPL() {
  repl.set(
    require("repl").start({
      prompt: "> ",
      input: socket,
      output: socket,
      useGlobal: true,
    }),
    refun(module, "setupREPL")
  );
});

defun(module, function setupREPL() {
  const repl = repl.get();
  repl.context.inModule = refun(module, "inModule");
});

defun(module, function inModule(name) {
  repl.get().module = require.cache[name];
  updatePropmpt();
});

defun(module, function updatePropmpt() {
  remote.setPrompt(`${remote.module.id}>`);
});
