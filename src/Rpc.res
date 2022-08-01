module Message = {
  type t<'a> = {
    method: string,
    params: array<'a>
  }

  let serialize = (t: t<'a>) => {
    "jsonrpc": "2.0",
    "method": t.method,
    "params": t.params
  }
  ->Js.Json.stringifyAny
  ->Belt.Option.getExn
}

module KeysMessage = {
  let toMessage: string => Message.t<string> = keys => {
    method: "keys",
    params: [ keys ],
  }

  let serialize = keys => keys->toMessage->Message.serialize
}

module ResizeMessage = {
  let message = {
    let maxUInt32 = 4_294_967_295.
    {
      method: "resize",
      params: [ maxUInt32, maxUInt32 ]
    }
    ->Message.serialize
  }
}