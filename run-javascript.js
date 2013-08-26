var system = require('system');

function inspect(object) {
  return JSON.stringify(object);
}

try {
  for (var i=1; i<system.args.length; i++) {
    var scriptFileName = system.args[i];
    console.log("Running " + scriptFileName + " ...");
    console.log("");
    var requireResult = require(scriptFileName);
    console.log("requireResult = " + inspect(requireResult));
  }
}
catch(error) {
  console.log(error);
  console.log(error.stack);
}
finally {
  phantom.exit();
}
