import * as epc from 'ts-elrpc';
import { encode, quote, symbol } from 'ts-elrpc';

/** @type {epc.RPCServer} */
let epc_client = null;

export async function init_epc_server() {
    if (epc_client === null) {
        try {
            epc_client = await epc.startServer();
        } catch(e) {
            console.error(e);
        }
    }
    return epc_client;
}

export function close_epc_client() {
    if (epc_client) {
        epc_client.stop()
    }
}

export function handle_arg_types(arg) {
    if (typeof arg === 'string' && arg.startsWith("'")) {
        arg = symbol(arg.split("'")[1]);
    }
    return quote(arg);
}

export async function eval_in_emacs(method_name, ...args) {
    const _args = [symbol(method_name), ...(args.map(handle_arg_types))];
    const sexp = encode(_args);
    return await epc_client.callMethod('eval-in-emacs', [sexp]);
}

export async function message_emacs(message) {
    eval_in_emacs("message", `[MD-PREVIEW] ${message}`);
}

export function convert_emacs_bool(symbol_value, symbol_is_boolean) {
    if (symbol_is_boolean == 't') {
        return symbol_value === 't';
    } else {
        return symbol_value;
    }
}

export async function get_emacs_vars(args) {
    const results = await epc_client.callMethod('get-emacs-vars', args)
    return results.map(([symbol_value, symbol_is_boolean]) => {
        return convert_emacs_bool(symbol_value, symbol_is_boolean);
    });
}

export async function get_emacs_var(var_name) {
    const result = await epc_client.callMethod('get-emacs-var', var_name);
    message_emacs("resutl", result)
    if (!result) {
        throw new Error(`[MD_PREVIEW] no such variable: ${var_name}`);
    }
    const [symbol_value, symbol_is_boolean] = result;
    return convert_emacs_bool(symbol_value, symbol_is_boolean)
}

export async function get_emacs_func_result(method_name, ...args) {
    const _args = [symbol(method_name), ...(args.map(handle_arg_types))];
    const sexp = encode(_args);
    const result = await epc_client.callMethod('get-emacs-func-result', [sexp])
    return result === 't' ? true : result;
}
