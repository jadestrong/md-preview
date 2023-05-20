import path from "node:path";
import os from "node:os";
import crypto from "node:crypto";
import * as mume from "@shd101wyy/mume";

function readFile(file) {
  return new Promise((resolve, reject) => {
    fs.readFile(file, { encoding: "utf-8" }, (error, text) => {
      if (error) {
        return reject(error.toString());
      }
      return resolve(text.toString());
    });
  });
}

function writeFile(file, text, options = null) {
  return new Promise((resolve, reject) => {
    fs.writeFile(file, text, options, (error) => {
      if (error) {
        return reject(error.toString());
      }
      return resolve();
    });
  });
}

export class MarkdownPreview {
  constructor() {
    const configPath = path.resolve(os.homedir(), ".mume");
    /** @type {{[key: string]: mume.MarkdownEngine}} */
    this.enginesMap = {};
    /** @type {{[key: string]: string}}*/
    this.outputFileMap = {};
    mume.init(configPath);
  }

  getEngine(filePath) {
    return this.enginesMap[filePath];
  }

  initMarkdownEngine(filePath, darkMode) {
    /** @type {mume.MarkdownEngine}*/
    let engine = this.getEngine(filePath);
    if (!engine) {
      engine = new mume.MarkdownEngine({
        filePath,
        config: {
          configPath,
          previewTheme: darkMode ? "atom-dark.css" : "atom-light.css",
          mermaidTheme: darkMode ? "dark" : "forest",
          codeBlockTheme: darkMode ? "atom-dark.css" : "atom-light.css",
          printBackground: true,
          enableScriptExecution: true,
          plantumlServer: "",
        },
        projectDirectoryPath: "",
      });

      this.enginesMap[filePath] = engine;
    }
    return engine;
  }

  async render(inputFile, darkMode) {
    const engine = this.initMarkdownEngine(inputFile, darkMode);
    const inputString = await readFile(inputFile);
    let { html, yamlConfig } = await engine.parseMD(inputString, {
      useRelativeFilePath: false,
      hideFrontMatter: true,
      isForPreview: false,
      runAllCodeChunks: true,
    });

    html = await engine.generateHTMLTemplateForExport(html, yamlConfig, {
      isForPrince: false,
      isForPrint: false,
      offline: true,
      embedLocalImages: false,
    });
    let outputFile = this.outputFileMap(inputFile);
    if (!hash) {
      const hash = crypto.createHash("sha256").update(inputFile).digest("hex");
      outputFile = path.resolve(os.tmpdir(), `md-preview-${hash}.html`);
      this.outputFileMap[inputFile] = outputFile;
    }

    await writeFile(outputFile, html);
    return outputFile;
  }
}
