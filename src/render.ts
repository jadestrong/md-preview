import http from 'node:http';
import path from 'node:path';
import { parse } from 'node:url';
import os from 'node:os';
import fs from 'node:fs';
import * as mume from '@shd101wyy/mume';

function readFile(file: string) {
  return new Promise<string>((resolve, reject) => {
    fs.readFile(file, { encoding: 'utf-8' }, (error, text) => {
      if (error) {
        return reject(error.toString());
      }
      return resolve(text.toString());
    });
  });
}

function writeFile(file: string  | number |Buffer | URL, text: any, options: fs.WriteFileOptions = null) {
  return new Promise<void>((resolve, reject) => {
    fs.writeFile(file, text, options, (error) => {
      if (error) {
        return reject(error.toString());
      }
      return resolve();
    });
  });
}

async function render(inputFile: string, outputFile: string, darkMode: boolean) {
  const configPath = path.resolve(os.homedir(), '.mume');
  await mume.init(configPath);

  const engine = new mume.MarkdownEngine({
    filePath: inputFile,
    config: {
      configPath,
      previewTheme: darkMode ? 'atom-dark.css' : 'atom-light.css',
      mermaidTheme: darkMode ? 'dark' : 'forest',
      codeBlockTheme: darkMode ? 'atom-dark.css' : 'atom-light.css',
      printBackground: true,
      enableScriptExecution: true,
      plantumlServer: '',
    },
    projectDirectoryPath: '',
  });

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

  await writeFile(outputFile, html);
}

async function server_handler(req: http.IncomingMessage, resp: http.ServerResponse) {
  console.log('req', req.url);
  if (req.url) {
    const q = parse(req.url, true).query;
    const inputFile = q.input_file as string;
    const outputFile = q.output_file as string;
    const darkMode = q.dark_mode === 'true';

    resp.writeHead(200, { 'Content-Type': 'text/plain' });
    try {
      await render(inputFile, outputFile, darkMode);
    } catch (err) {
      if (err instanceof Error) {
        resp.end(err.message);
      } else {
        resp.end(err);
      }

    }
  }
}

export async function main() {
  const argv = process.argv;
  console.log(argv);
  const port = Number(argv[2]);

  http.createServer(server_handler).listen(port);
}

main()
