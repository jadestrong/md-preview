import process from "node:process";
import {
  eval_in_emacs,
  get_emacs_func_result,
  init_epc_server,
  message_emacs,
} from "./utils.mjs";
import { MarkdownPreview } from "./markdown-engine.mjs";

class MdPreview {
  constructor() {
    this.darkMode = true;
    Promise.all([this.init(), this.initEngine()]).then(this.initedSuccess);
  }

  async init() {
    const client = await init_epc_server();
    client.defineMethod("echo", (...args) => {
      return args;
    });
    client.defineMethod("preview", this.preview);
    client.defineMethod("open", this.open);
  }

  async initEngine() {
    this.engine = new MarkdownPreview();
  }
  initedSuccess = async () => {
    const darkMode = await get_emacs_func_result("md-preview--is-dark-theme");
    this.darkMode = darkMode;
  };

  echo(...args) {
    return args;
  }

  preview = async (inputFile) => {
    const outputFile = await this.engine.render(inputFile, this.darkMode);
    eval_in_emacs("md-preview-show-preview-window", outputFile);
  };

  open = async (inputFile) => {
    const outputFile = await this.engine.render(inputFile, this.darkMode);
    return outputFile;
  };
}

function main() {
  new MdPreview();
}

main();

process.on("uncaughtException", (err) => {
  try {
    message_emacs("uncaughtException", err.stack);
  } catch (e) {
    console.log("e", e);
  }
  console.log("uncaught exception err", err);
});

process.on("unhandledRejection", (reason, p) => {
  if (reason instanceof Error) {
    message_emacs("UnhandleRejection: " + reason.message + "\n" + reason.stack);
  } else {
    message_emacs("UnhandleRejection: " + JSON.stringify(reason));
  }
  console.log("unhandledRejection err", reason, p);
});
