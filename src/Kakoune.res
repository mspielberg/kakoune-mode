let editorSession = ref(ClientSession.make(
  [ "-clear" ],
))

let outputSession = ClientSession.make(
  [
    "-ui", "json",
    "-c" , "vscode",
    "-e", "rename-client vscodeoutput",
  ],
)

let writeToKak = message => {
  Js.log2("kak <", message)
  editorSession.contents
    ->ClientSession.write(message->Rpc.Message.serialize)
}

type selectionUpdateState =
| ReadyToDraw
| NeedUpdate
| AwaitingSelections

let needSelectionsUpdate = ref(NeedUpdate)

let writeKeysInternal = keys => keys->Rpc.KeysMessage.make->writeToKak

let writeKeys = keys => {
  writeKeysInternal(keys)
  needSelectionsUpdate.contents = NeedUpdate
}

let querySelectionsCommand = ":evaluate-commands -client vscodeoutput echo kakoune-mode selections: %val{selections_char_desc}<ret>"

let querySelections = () => {
  outputSession->ClientSession.start->ignore
  switch Mode.getMode() {
  | Mode.Normal => writeKeysInternal(querySelectionsCommand)
  | Mode.Insert => writeKeysInternal("<a-;>" ++ querySelectionsCommand)
  | _ => ()
  }
  needSelectionsUpdate.contents = AwaitingSelections
}

let getAtomStartColumns = (line: KakouneTypes.Line.t) => line->Js.Array2.reduce((starts, atom) => {
    open Js.Array2
    if length(starts) == 0 {
      [(0, Js.String2.length(atom.contents))] // first atom starts at column 0
    } else {
      let (prevStart, prevEnd) = starts[length(starts) - 1]
      starts->push((prevStart + prevEnd, prevEnd + atom.contents->Js.String2.length))->ignore
      starts
    }
  }, [])
  ->Js.Array2.map(((start, _)) => start)

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
  if modeLine[4].contents == " param=" {
    Mode.EnterParam
  } else {
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

let processDrawStatus = (statusLine, modeLine) => {
  let newMode = modeLine->getModeFromModeLine
  newMode->Mode.setMode
  newMode->Vscode.updateCursorStyle
  switch Mode.getMode() {
  | Mode.Unknown
  | Mode.Normal
  | Mode.Insert
  | Mode.EnterKey
  | Mode.EnterParam => ()
  | Mode.Prompt => updatePrompt(statusLine)
  }
  Promise.resolve()
}

let currentSelections: ref<array<KakouneTypes.Selection.t>> = ref([])

let processOutputDrawStatus = (statusLine, _) => {
  switch statusLine->getSelectionsFromStatusLine {
  | Some(selections) =>
    currentSelections.contents = selections
    needSelectionsUpdate.contents = ReadyToDraw
  | _ => ()
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

let processRefresh = () => switch needSelectionsUpdate.contents {
  | NeedUpdate =>
    querySelections()
    Promise.resolve()
  | AwaitingSelections =>
    Promise.resolve()
  | ReadyToDraw =>
    if currentSelections.contents->Js.Array2.length > 0 {
      Some(currentSelections.contents[0])
    } else {
      None
    }
    ->Belt.Option.flatMap(mainSelection => {
      drawBuffer.contents[mainSelection.cursor.line]
        ->getCursorPositionFromLine
        ->Belt.Option.map(columnInDrawBuffer => {
          drawBuffer.contents
          ->trimLeft(columnInDrawBuffer - mainSelection.cursor.column)
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

let processCommand = (req: Rpc.UIRequest.t) => switch req {
| Draw({lines: lines}) => processDraw(lines)
| DrawStatus({statusLine: statusLine, modeLine: modeLine}) =>
  processDrawStatus(statusLine, modeLine)
| InfoHide => processInfoHide()
| InfoShow({title: title, content: content, style: style}) =>
  processInfoShow(title, content, style)
| Refresh =>
  processRefresh()
| SetCursor({mode: mode, coord: coord}) =>
  processSetCursor(mode, coord)
}

let processOutputSessionRequest = (req: Rpc.UIRequest.t) => switch req {
| DrawStatus({statusLine: statusLine, modeLine: modeLine}) =>
  processOutputDrawStatus(statusLine, modeLine)
| Refresh =>
  processRefresh()
| _ => Promise.resolve()
}

let initKak = (filenameOpt: option<string>) => {
  let args = switch filenameOpt {
  | Some(filename) => [ "-ui", "json", "-s", "vscode", filename ]
  | None => [ "-ui", "json", "-s", "vscode" ]
  }
  let session = ClientSession.make(args)
  editorSession.contents = session
  session
    ->ClientSession.setHandler(processCommand)
    ->ClientSession.start
    ->ignore
  outputSession
    ->ClientSession.setHandler(processOutputSessionRequest)
    ->ignore
  writeToKak(Rpc.ResizeMessage.make())
}
