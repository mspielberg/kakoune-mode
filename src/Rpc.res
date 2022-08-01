type keysMessage = {
  jsonrpc: string,
  method: string,
  params: array<string>,
}

let createKeysMessage = keys => {
  let correctedKeys = keys|>Js.String.replace("\n", "<ret>")
  {jsonrpc: "2.0", method: "keys", params: [correctedKeys]}
}

module Encode = {
  let keysMessage = km => {
    {
      "jsonrpc": km.jsonrpc,
      "method": km.method,
      "params": km.params,
    }
  }
  let resizeMessage = {
    let maxUInt32 = 4_294_967_295.
    {
      "jsonrpc": "2.0",
      "method": "resize",
      "params": [ maxUInt32, maxUInt32 ]
    }
  }
}

let maxResizeMessage = Encode.resizeMessage->Js.Json.stringifyAny->Belt.Option.getExn

let stringifyMessage = msg => msg->Encode.keysMessage->Js.Json.stringifyAny->Belt.Option.getExn
