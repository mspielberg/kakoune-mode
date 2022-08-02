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
  | "prompt" => Mode.Prompt
  | s if Js.String2.includes(s, " sel") => Mode.Normal
  | s => {
      Js.log2("Unknown modeline entry:", s)
      Mode.Unknown
    }
  }

let updatePrompt = (statusLine: KakouneTypes.Line.t) => {
  let prompt = statusLine[0].contents
  let content = statusLine[1].contents
  Vscode.showPrompt(prompt, content, writeKeys)
}

let processDraw = (lines: array<KakouneTypes.Line.t>) =>
  switch Mode.getMode() {
  | _ => 
    lines
    ->Js.Array2.map(KakouneTypes.Line.getText)
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
    | Mode.Prompt => updatePrompt(statusLine)
    }
  }
  Promise.resolve()
}

let infoActive = ref(false)

let processInfoHide = () => {
  if infoActive.contents {
    infoActive.contents = false
    Vscode.hidePrompt()
  }
  Promise.resolve()
}

let showEnterKeyPrompt = (title, content) => {
  let title = KakouneTypes.Line.getText(title)
  let options = content->Js.Array2.map(line => {
    let components = line->KakouneTypes.Line.getText->Js.String2.split(": ")
    (components[0], components[1])
  })
  infoActive.contents = true
  Vscode.showEnterKeyPrompt(title, options)
}

let processInfoShow = (title, content, infoStyle) => {
  switch infoStyle {
  | #prompt if Mode.getMode() == Mode.EnterKey => showEnterKeyPrompt(title, content, writeKeys)
  | _ => ()
  }
  Promise.resolve()
}

let processSetCursor = (mode, coord) => {
  if mode == "buffer" && Mode.getMode() == Mode.Insert {
    let vscodePosition = coord->KakouneTypes.Coord.toVscode
    Vscode.Selection.make(~anchor=vscodePosition, ~active=vscodePosition)
    ->Vscode.setSelection
  }
  Promise.resolve()
}

let processCommand = (msg: string) => {
  let parsed = msg->Rpc.UIRequest.parse
  switch parsed {
    | Some(Draw({lines: lines})) => processDraw(lines)
    | Some(DrawStatus({statusLine: statusLine, modeLine: modeLine})) =>
      processDrawStatus(statusLine, modeLine)
    | Some(InfoHide) => processInfoHide()
    | Some(InfoShow({title: title, content: content, style: style})) =>
      processInfoShow(title, content, style)
    | Some(SetCursor({mode: mode, coord: coord})) =>
      processSetCursor(mode, coord)
    | None => Promise.resolve()
  }
}

let handleIncomingError = error => {
  let str = error->Js.String2.fromCharCodeMany
  Js.log2("kakerr >", str)
  str->Vscode.Window.showError
}

let pendingCommand = ref(Promise.resolve())

let handleIncomingCommand = command =>
  command
  ->Js.String2.fromCharCodeMany
  ->Rpc.InputBuffer.push(msg => {
    pendingCommand.contents = pendingCommand.contents->Promise.then(_ => processCommand(msg))
  })
