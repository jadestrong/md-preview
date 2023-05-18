import { init_epc_server } from './utils.mjs'

class MdPreview {
    constructor() {
        init_epc_server().then(epc_client => {
            epc_client.defineMethod('echo', (...args) => {
                return args;
            });
        });
    }
}

function main() {
    new MdPreview();
}

main()
