let initKak = writeToKak => Rpc.ResizeMessage.make()->writeToKak

let kak = ref(Node.spawn("kak", ["-clear"]))

let getKak = () => kak.contents

let setKak = newKak => kak := newKak

let writeToKak = message => {
  Js.log2("kak <", message)
  getKak().stdin.write(. message->Rpc.Message.serialize)
}

let writeKeys = keys => keys->Rpc.KeysMessage.make->writeToKak

let querySelections = () =>
  writeKeys(":echo kakoune-mode selections: %val{selections_display_column_desc}<ret>")

let getSelectionsFromStatusLine = (statusLine: KakouneTypes.Line.t) => {
  let prefix = "kakoune-mode selections: "
  if Js.Array2.length(statusLine) > 0 {
    open KakouneTypes.Selection
    let contents = statusLine[0].contents
    if contents->Js.String2.startsWith(prefix) {
      contents
      ->Js.String2.substr(~from=prefix->Js.String2.length)
      ->selectionsFromDescString
      ->Js.Array2.map(toVscode)
      ->Some
    } else {
      None
    }
  } else {
    None
  }
}

let getModeFromModeLine = (modeLine: KakouneTypes.Line.t) =>
  switch modeLine[3].contents
  {
  | "insert" => Mode.Insert
  | "enter key" => Mode.EnterKey
  | s if Js.String2.includes(s, " sel") => Mode.Normal
  | s => {
      Js.log2("Unknown modeline entry:", s)
      Mode.Unknown
    }
  }

let processDraw = (lines: array<KakouneTypes.Line.t>) =>
  switch Mode.getMode() {
  | Mode.Insert => () // Nothing to do.
  | Mode.Unknown
  | Mode.Normal
  | Mode.EnterKey => 
    lines
    ->Js.Array2.map(l => Some(l)->KakouneTypes.Line.getText->Belt.Option.getExn)
    ->Js.String2.concatMany("", _)
    ->Vscode.replaceAll
  }

let processDrawStatus = (statusLine, modeLine) => {
  let newMode = modeLine->getModeFromModeLine
  newMode->Mode.setMode
  newMode->Vscode.updateCursorStyle
  switch statusLine->getSelectionsFromStatusLine {
  | Some(selections) => Vscode.setSelections(selections)
  | None => switch Mode.getMode() {
    | Mode.Normal => querySelections()
    | Mode.Unknown
    | Mode.Insert
    | Mode.EnterKey => ()
    }
  }
}

let processSetCursor = (mode, coord) => {
  if mode == "buffer" && Mode.getMode() == Mode.Insert {
    let vscodePosition = coord->KakouneTypes.Coord.toVscode
    VscodeTypes.Selection.make(~anchor=vscodePosition, ~active=vscodePosition)
    ->Vscode.setSelection
  }
}

let processCommand = (msg: string) => {
  let parsed = msg->Rpc.UIRequest.parse
  Js.log2("kak >", parsed)
  switch parsed {
    | Some(Draw({lines: lines})) => processDraw(lines)
    | Some(DrawStatus({statusLine: statusLine, modeLine: modeLine})) =>
      processDrawStatus(statusLine, modeLine)
    | Some(SetCursor({mode: mode, coord: coord})) =>
      processSetCursor(mode, coord)
    | None => ()
  }
}

let handleIncomingError = error => {
  let str = error->Js.String2.fromCharCodeMany
  Js.log2("kakerr >", str)
  str->VscodeTypes.Window.showError
}

let handleIncomingCommand = command =>
  command
  ->Js.String2.fromCharCodeMany
  ->Js.String2.split("\n")
  ->Js.Array2.filter(s => s->Js.String2.length > 0)
  ->Js.Array2.forEach(processCommand)
