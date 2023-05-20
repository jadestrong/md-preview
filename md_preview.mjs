import process from "node:process";
import {
  get_emacs_func_result,
  init_epc_server,
  message_emacs,
} from "./utils.mjs";
import { MarkdownPreview } from "./markdown-engine.mjs";

class MdPreview {
  constructor() {
    Promise.all([this.init(), this.initEngine()]).then(this.initedSuccess);
  }

  async init() {
    const client = await init_epc_server();
    client.defineMethod("echo", (...args) => {
      return args;
    });
  }

  async initEngine() {
    this.engine = new MarkdownPreview();
  }
  async initedSuccess() {
    const darkMode = await get_emacs_func_result("md-preview--is-dark-theme");
      // update config
  }

  echo(...args) {
    return args;
  }
}

function main() {
  new MdPreview();
}

main();

process.on("uncaughtException", (err) => {
  message_emacs("uncaughtException", err.stack);
});

process.on("unhandledRejection", (reason, p) => {
  if (reason instanceof Error) {
    message_emacs("UnhandleRejection: " + reason.message + "\n" + reason.stack);
  } else {
    message_emacs("UnhandleRejection: " + JSON.stringify(reason));
  }
});
