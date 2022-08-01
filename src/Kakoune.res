type color = string
type attribute = string
type face = {
  fg: color,
  bg: color,
  attributes: array<attribute>,
}
type atom = {
  face: face,
  contents: string,
}
type line = array<atom>
type coord = {
  line: int,
  column: int,
}

type drawStatusCommand = (line /* status_line */, line /* mode_line */, face /* default_face */)
type drawCommand = (array<line> /* lines */, face /* default_face */, face /* padding_face */)
type setCursorCommand = (string /* mode */, coord)

module Decode = {
  open Belt.Option
  open Js.Json

  let array = (json, f) =>
    json
    ->decodeArray
    ->getExn
    ->Js.Array2.map(f)

  let field = (data, name, f) => {
    data
    ->decodeObject
    ->flatMap(Js.Dict.get(_, name))
    ->getExn
    ->f
  }
  
  let int = json =>
    json
    ->decodeNumber
    ->getExn
    ->Belt.Float.toInt

  let string = json =>
    json
    ->decodeString
    ->getExn

  let tuple2 = (json, f1, f2) => {
    let arr = json
      ->decodeArray
      ->getExn
    ( f1(arr[0]), f2(arr[1]) )
  }

  let tuple3 = (json, f1, f2, f3) => {
    let arr = json
      ->decodeArray
      ->getExn
    ( f1(arr[0]), f2(arr[1]), f3(arr[2]) )
  }

  let method_ = json => {
    json
    ->field("method", string)
  }

  let face = json => {
    {
      fg: json->field("fg", string),
      bg: json->field("bg", string),
      attributes: json->field("attributes", array(_, string)),
    }
  }

  let atom = json => {
    {
      face: json->field("face", face),
      contents: json->field("contents", string),
    }
  }

  let coord = json => {
    {
      line: json->field("line", int),
      column: json->field("column", int),
    }
  }

  let line = json => {
    json->array(atom)
  }

  let drawCommand: Js.Json.t => drawCommand = json => {
    json->field("params", tuple3(_, array(_, line), face, face))
  }

  let drawStatusCommand: Js.Json.t => drawStatusCommand = json => {
    json->field("params", tuple3(_, line, line, face))
  }

  let setCursorCommand: Js.Json.t => setCursorCommand = json => {
    json->field("params", tuple2(_, string, coord))
  }
}

module Line = {
  open Belt.Option
  type t = array<atom>

  let getAtomsBeforeAtomIndex = (line: option<t>, atomIndex) =>
    switch (line, atomIndex) {
    | (Some(l), Some(i)) => Some(l->Js.Array2.slice(~start=0, ~end_=i))
    | _ => None
    }

  let getAtomIndexBy = (line: option<t>, f) =>
    line
    ->Belt.Option.flatMap(Belt.Array.getIndexBy(_, f))

  let getAtomsBeforeAtom = (line, a) =>
    line
    ->getAtomIndexBy(atom => eq(Some(atom), a, \"=="))
    ->getAtomsBeforeAtomIndex(line, _)

  let getAtomBy = (line, f) => line->flatMap(Belt.Array.getBy(_, f))

  let reverse = line => line->map(Belt.Array.reverse)

  let getText = line => line->map(Js.Array2.reduce(_, (lineText, atom) => lineText ++ atom.contents, ""))

  let getNumberOfAtoms = map(_, Js.Array2.length)

  let getLineLength = line => line->map(Js.Array2.reduce(_, (lineLength, atom) => lineLength + Js.String.length(atom.contents), 0))
}

module Document = {
  type t = array<line>

  let getLine = (lines: t, lineNumber: option<int>) => lineNumber->Belt.Option.flatMap(lines->Belt.Array.get(_))

  let getLineBy = (lines: t, f) => lines->Belt.Array.getBy(f)

  let getLineThatHasAtom = (lines: t, atom: option<atom>) =>
    lines->getLineBy(l => l->Js.Array2.some(a => Belt.Option.eq(Some(a), atom, \"==")))

  let getLineIndexBy = (lines: t, f) => lines->Belt.Array.getIndexBy(f)
}

module Position = {
  type t = coord

  let toVscode = p => Vscode.Position.make(~line=p.line, ~character=p.column)

  let fromString = str => {
    let components = str->Js.String2.split(".")
    {
      line: components[0]->Belt.Int.fromString->Belt.Option.getExn - 1,
      column: components[1]->Belt.Int.fromString->Belt.Option.getExn - 1,
    }
  }

  let successor = p => {
    line: p.line,
    column: p.column + 1,
  }

  let isAfter = (p1, p2) =>
    p1.line > p2.line
    || p1.line == p2.line && p1.column > p2.column
}

module Selection = {
  type t = {
    anchor: Position.t,
    cursor: Position.t,
  }

  let isForward = s => s.cursor->Position.isAfter(s.anchor)

  let fromString = str => {
    let components = str->Js.String2.split(",")
    {
      anchor: Position.fromString(components[0]),
      cursor: Position.fromString(components[1]),
    }
  }

  let toVscode = selection => {
    if selection->isForward {
      Vscode.Selection.make(
        ~anchor=selection.anchor->Position.toVscode,
        ~active=selection.cursor->Position.toVscode)
    } else {
      Vscode.Selection.make(
        ~anchor=selection.anchor->Position.successor->Position.toVscode,
        ~active=selection.cursor->Position.toVscode)
    }
  }

  let selectionsFromString = str => {
    str
    ->Js.String2.split(" ")
    ->Js.Array2.map(fromString)
  }
}

let initKak = writeToKak => Rpc.ResizeMessage.message->writeToKak

let kak = ref(Node.spawn("kak", ["-clear"]))

let getKak = () => kak.contents

let setKak = newKak => kak := newKak

let writeToKak = message => {
  Js.log2("kak <", message)
  getKak().stdin.write(. message)
}

let writeKeys = keys => keys->Rpc.KeysMessage.serialize->writeToKak

let querySelections = () =>
  writeKeys(":echo kakoune-mode selections: %val{selections_display_column_desc}<ret>")

let getSelectionsFromStatusLine = statusLine => {
  let prefix = "kakoune-mode selections: "
  if statusLine->Js.String2.startsWith(prefix) {
    statusLine
    ->Js.String2.substr(~from=prefix->Js.String2.length)
    ->Selection.selectionsFromString
    ->Js.Array2.map(Selection.toVscode)
    ->Some
  } else {
    None
  }
}

let getSelectionsFromDrawStatus = drawStatusCommand => {
  let (statusLine, _, _) = drawStatusCommand
  if Js.Array2.length(statusLine) > 0 {
    statusLine[0].contents->getSelectionsFromStatusLine
  } else {
    None
  }
}

let getModeFromModeLine = modeLine =>
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

let getModeFromDrawStatus = drawStatusCommand => {
  let (_, modeLine, _) = drawStatusCommand
  modeLine->getModeFromModeLine
}

type searchDirection =
  | Left
  | Right

type atomPosition = {
  line: int,
  atom: int,
}

let updateLine = (line, lineNumber) => {
  line->Some->Line.getText->Belt.Option.getExn->Vscode.replaceLine(lineNumber)
}

let processDraw = drawCommand => {
  let (lines, _, _) = drawCommand
  lines
  ->Js.Array2.map(l => Some(l)->Line.getText->Belt.Option.getExn)
  ->Js.String2.concatMany("", _)
  ->Vscode.replaceAll
}

let processDrawStatus = drawStatusCommand => {
  let newMode = drawStatusCommand->getModeFromDrawStatus
  newMode->Mode.setMode
  newMode->Vscode.updateCursorStyle
  switch drawStatusCommand->getSelectionsFromDrawStatus {
  | Some(selections) => Vscode.setSelections(selections)
  | None => switch Mode.getMode() {
    | Mode.Normal => querySelections()
    | Mode.Unknown
    | Mode.Insert
    | Mode.EnterKey => ()
    }
  }
}

let processSetCursor = setCursorCommand => {
  let (mode, position) = setCursorCommand
  if mode == "buffer" && Mode.getMode() == Mode.Insert {
    let vscodePosition = position->Position.toVscode
    Vscode.Selection.make(~anchor=vscodePosition, ~active=vscodePosition)
    ->Vscode.setSelection
  }
}

let processCommand = (msg: string) => {
  let parsed = msg->Js.Json.parseExn
  Js.log2("kak >", parsed)
  switch parsed->Decode.method_ {
  | "draw" =>
    switch Mode.getMode() {
    | Mode.Unknown
    | Mode.Normal
    | Mode.EnterKey => switch parsed->Decode.drawCommand {
      | dc => processDraw(dc)
      | exception e => e->Js.log
      }
    | Mode.Insert => () // Nothing to do.
    }
  | "draw_status" =>
    switch parsed->Decode.drawStatusCommand {
    | dsc => processDrawStatus(dsc)
    | exception e => e->Js.log
    }
  | "set_cursor" =>
    switch parsed->Decode.setCursorCommand {
    | sc => processSetCursor(sc)
    | exception e => e->Js.log
    }
  | exception e => e->Js.log
  | _ => ()
  }
}

let handleIncomingError = error => {
  let str = error->Js.String2.fromCharCodeMany
  Js.log2("kakerr >", str)
  str->Vscode.Window.showError
}

let handleIncomingCommand = command =>
  command
  ->Js.String2.fromCharCodeMany
  ->Js.String2.split("\n")
  ->Js.Array2.filter(s => s->Js.String2.length > 0)
  ->Js.Array2.forEach(processCommand)
