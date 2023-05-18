const { spawn } = require("child_process");
const http = require("http");

const port = 8081;
function sendRequest(params) {
  const options = {
    hostname: "127.0.0.1",
    port,
    path: `/?output_file=${params.outputFile}&input_file=${params.inputFile}&dark_mode=${params.darkMode}`,
    method: "GET",
  };

  const req = http.request(options, (res) => {
    console.log(`statusCode: ${res.statusCode}`);

    res.on("data", (d) => {
      process.stdout.write(d);
    });
  });

  req.on("error", (error) => {
    console.error(error);
  });

  req.end();
}

function main() {
  try {
    const child = spawn("npx", ["ts-node", "./src/render.ts", port]);
    // Listen for data from the child process
    child.stdout.on("data", (data) => {
      console.log(`Child process output: ${data}`);
    });

    // Listen for errors from the child process
    child.stderr.on("data", (data) => {
      console.error(`Child process error: ${data}`);
    });

    // Listen for the child process to exit
    child.on("exit", (code, signal) => {
      console.log(
        `Child process exited with code ${code} and signal ${signal}`
      );
    });

    // Send data to the child process
    child.stdin.write("Hello, child process!");
    console.log("success!");
    setTimeout(() => {
      console.log("send a request");
      sendRequest({
        inputFile:
          "/Users/jadestrong/Documents/Github/rust-for-typescript-devs/lessons/01-introduction/A-intro.md",
        outputFile: "/Users/jadestrong/Documents/Github/md-preview/test.html",
        dark_mode: true,
      });
    }, 5000);
  } catch (e) {
    console.error(e);
  }
}

main();
