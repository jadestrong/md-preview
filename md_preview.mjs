import { get_emacs_var, init_epc_server } from "./utils.mjs";
import { initMarkdownEngine } from "./markdown-engine.mjs";

class MdPreview {
  constructor() {
      Promise.all([
          this.init(),
          this.initEngine(),
      ]).then(this.initSuccess)
  }

  async init() {
    const client = await init_epc_server();
    client.defineMethod("echo", this.echo);
  }

  async initEngine() {
    this.engine = await initMarkdownEngine();
  }

    async initSuccess() {
        await get_emacs_var()
    }

  echo(...args) {
    return args;
  }
}

function main() {
  new MdPreview();
}

main();
