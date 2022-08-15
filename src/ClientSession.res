module InputBuffer = {
  type t = {
      buf: ref<string>,
  }

  let make = () => { buf: ref("") }

  let rec dispatch = (t, f: Rpc.UIRequest.t => unit) => {
    let index = Js.String2.indexOf(t.buf.contents, "\n")
    if index >= 0 {
      let obj = t.buf.contents->Js.String2.substring(~from=0, ~to_=index)
      obj->Rpc.UIRequest.parse->Belt.Option.forEach(f)
      t.buf.contents = t.buf.contents->Js.String2.substr(~from=index + 1)
      dispatch(t, f)
    }
  }
  
  let push = (t, str, f) => {
    t.buf.contents = t.buf.contents ++ str
    dispatch(t, f)
  }
}

type t = {
  id: string,
  args: array<string>,
  mutable childProcess: option<Node.child_process>,
  inputBuffer: InputBuffer.t,
  mutable requestHandler: Rpc.UIRequest.t => Promise.t<unit>,
  pendingCommand: ref<Promise.t<unit>>,
}

let getPid = t => switch t.childProcess {
| Some(child) => Js.Int.toString(child.pid)
| None => "(not started)"
}

let handleIncomingBytes = (t, bytes) => {
  Js.log4("Received from", t.id, Js.Array2.length(bytes), "bytes")
  let str = Js.String2.fromCharCodeMany(bytes)
  t.inputBuffer->InputBuffer.push(str, msg => {
    t.pendingCommand.contents = t.pendingCommand.contents
      ->Promise.then(_ => {
        // Js.log2("invoking handler with", msg)
        t.requestHandler(msg)
      })
      ->Promise.catch(e => {
        Js.log3("command handler failed", msg, e)
        Promise.resolve()
      })
  })
}

let write = (t, s) => {
  let success = Belt.Option.getExn(t.childProcess).stdin.write(. s)
  switch success {
  | true => Js.log4("Successfully wrote to", t.id, Js.String2.length(s), "chars")
  | false => Js.log3("Failed writing", Js.String2.length(s), "chars")
  }
}

let make = (id, args) => {
  id: id,
  args: args,
  childProcess: None,
  inputBuffer: InputBuffer.make(),
  requestHandler: _ => Promise.resolve(),
  pendingCommand: ref(Promise.resolve()),
}

let setHandler = (t, requestHandler) => {
  t.requestHandler = requestHandler
  t
}

let start = (t) => {
  if Belt.Option.isNone(t.childProcess) {
    let child = Node.spawn("kak", t.args)
    t.childProcess = child->Some
    child.stdout.on(. "data", handleIncomingBytes(t, _))
    child.stderr.on(. "data", bytes => Js.log2("kakerr >", Js.String.fromCharCodeMany(bytes)))
  }
  t
}
