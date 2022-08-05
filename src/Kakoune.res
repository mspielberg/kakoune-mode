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

let getAtomStartColumns = (line: KakouneTypes.Line.t) => line->Js.Array2.reduce((starts, atom) => {
  switch starts->Js.Array2.pop {
  | None => [0] // first atom starts at column 0
  | Some(prev) =>
    starts->Js.Array2.push(prev)->ignore
    starts->Js.Array2.push(prev + atom.contents->Js.String2.length)->ignore
    starts
  }
}, [])

let isCursorFace = (face: KakouneTypes.Face.t) =>
  face.fg == "black" && (face.bg == "white" || face.bg == "cyan")

let getCursorPositionFromLine = (line: KakouneTypes.Line.t) => {
  let index = line->Js.Array2.findIndex(a => a.face->isCursorFace)
  if index < 0 {
    None
  } else {
    Some(getAtomStartColumns(line)[index])
  }
}

let getSelectionsFromStatusLine = (statusLine: KakouneTypes.Line.t) => {
  let prefix = "kakoune-mode selections: "
  if Js.Array2.length(statusLine) > 0 {
    open KakouneTypes.Selection
    let contents = statusLine[0].contents
    if contents->Js.String2.startsWith(prefix) {
      contents
      ->Js.String2.substr(~from=prefix->Js.String2.length)
      ->selectionsFromDescString
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

let drawBuffer: ref<array<KakouneTypes.Line.t>> = ref([])

let processDraw = (lines: array<KakouneTypes.Line.t>) => {
  drawBuffer.contents = lines
  Promise.resolve()
}

let currentSelections: ref<array<KakouneTypes.Selection.t>> = ref([])

let processDrawStatus = (statusLine, modeLine) => {
  let newMode = modeLine->getModeFromModeLine
  newMode->Mode.setMode
  newMode->Vscode.updateCursorStyle
  switch statusLine->getSelectionsFromStatusLine {
  | Some(selections) => currentSelections.contents = selections
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

let trimLeft = (lines, n) => {
  lines
  ->Js.Array2.map(line =>
    line
    ->KakouneTypes.Line.getText
    ->Js.String2.substringToEnd(~from=n)
  )
  ->Js.Array2.joinWith("")
}

let processRefresh = () => {
  if currentSelections.contents->Js.Array2.length > 0 {
    Some(currentSelections.contents[0])
  } else {
    None
  }
  ->Belt.Option.flatMap(mainSelection => {
    drawBuffer.contents[mainSelection.cursor.line]
      ->getCursorPositionFromLine
      ->Belt.Option.map(column => {
        let columnOffset = mainSelection.cursor.column - column
        drawBuffer.contents
        ->trimLeft(columnOffset)
        ->Vscode.replaceAll
        ->Promise.thenResolve(_ =>
          currentSelections.contents
          ->Js.Array2.map(Vscode.Selection.fromKakoune)
          ->Vscode.setSelections
        )
        ->Promise.catch(e => {
          Js.log2("replaceAll failed", e)
          Promise.resolve()
        })
      })
  })
  ->Belt.Option.getWithDefault(Promise.resolve())
}

let processSetCursor = (mode, coord) => {
  if mode == "buffer" && Mode.getMode() == Mode.Insert {
    let vscodePosition = coord->Vscode.Position.fromKakoune
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
    | Some(Refresh) =>
      processRefresh()
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
    pendingCommand.contents = pendingCommand.contents
      ->Promise.then(_ => processCommand(msg))
      ->Promise.catch(e => {
        Js.log3("command handler failed", msg, e)
        Promise.resolve()
      })
  })
