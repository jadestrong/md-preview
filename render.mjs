import path from "node:path";
import os from "node:os";
import * as mume from "@shd101wyy/mume";

let engine;

export async function getEngine(darkMode) {
  if (!engine) {
    const configPath = path.resolve(os.homedir(), ".mume");
    await mume.init(configPath);

    engine = new mume.MarkdownEngine({
      filePath: inputFile,
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
  }
  return engine;
}

// export const getEngine();
