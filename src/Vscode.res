open VscodeTypes

@module external vscode: vscode = "vscode"

module Commands = {
  @send external registerCommand: (commands, string, 'a => unit) => disposable = "registerCommand"

  let registerCommand: (string, 'a => unit) => disposable = (name, callback) =>
    vscode.commands->registerCommand(name, callback)

  @send external executeCommand: (commands, string) => unit = "executeCommand"

  let executeCommand: string => unit = command =>
    vscode.commands->executeCommand(command)

  @send external executeCommandWithArg: (commands, string, textCommandArgs) => unit = "executeCommand"

  let executeCommandWithArg: (string, textCommandArgs) => unit = (command, arg) =>
    vscode.commands->executeCommandWithArg(command, arg)
}

module OutputChannel = {
  @send external append: (outputChannel, string) => unit = "append"
  @send external appendLine: (outputChannel, string) => unit = "appendLine"
}

module QuickPick = {
  type quickPickItem = {
    "label": string,
    "description": string,
  }

  type t = {
    activeItems: array<quickPickItem>,
    title: string,
    items: array<quickPickItem>,
    value: string,
  }

  @send external createQuickPick: window => t = "createQuickPick"

  let make = () => vscode.window->createQuickPick

  @set external setItems: (t, array<quickPickItem>) => unit = "items"
  @set external setPlaceholder: (t, string) => unit = "placeholder"
  @set external setTitle: (t, string) => unit = "title"
  @set external setValue: (t, string) => unit = "value"

  @send external onDidAccept: (t, unit => unit) => unit = "onDidAccept"
  @send external onDidChangeActive: (t, array<quickPickItem> => unit) => unit = "onDidChangeActive"
  @send external onDidChangeSelection: (t, array<quickPickItem> => unit) => unit = "onDidChangeSelection"
  @send external onDidChangeValue: (t, string => unit) => unit = "onDidChangeValue"
  @send external onDidHide: (t, unit => unit) => unit = "onDidHide"
  @send external dispose: t => unit = "dispose"
  @send external show: t => unit = "show"
}

module TextDocument = {
  type t = textDocument

  @send external getText: t => string = "getText"
}

module TextEditor = {
  type t = textEditor
  type editBuilder
  type editOptions = {
    undoStopAfter: bool,
    undoStopBefore: bool,
  }

  @deriving(jsConverter)
  type cursorStyle =
    | @as(1) Line
    | @as(2) Block
    | @as(3) Underline
    | @as(4) LineThin
    | @as(5) BlockOutline
    | @as(6) UnderlineThin

  let activeTextEditor = () => vscode.window.activeTextEditor

  let options = () => activeTextEditor()->Belt.Option.map(t => t.options)

  let document = () => activeTextEditor()->Belt.Option.map(t => t.document)

  @get external getSelection: t => selection = "selection"
  @set external setSelection: (t, selection) => unit = "selection"
  @get external getSelections: t => array<selection> = "selections"
  @set external setSelections: (t, array<selection>) => unit = "selections"
  @send external edit: (t, editBuilder => unit, editOptions) => Promise.t<bool> = "edit"
  @send external revealRange: (t, range) => unit = "revealRange"

  @send external delete: (editBuilder, range) => unit = "delete"
  @send external insert: (editBuilder, position, string) => unit = "insert"
  @send external replace: (editBuilder, selection, string) => unit = "replace"
}

module Window = {
  @send external createOutputChannel: (window, string) => disposable = "createOutputChannel"

  let createOutputChannel = name => vscode.window->createOutputChannel(name)

  @send external showErrorMessage: (window, string) => unit = "showErrorMessage"

  let showError: string => unit = message => vscode.window->showErrorMessage(message)

  @send external onDidChangeActiveTextEditor: (window, option<TextEditor.t> => unit) => unit = "onDidChangeActiveTextEditor"

  let onDidChangeActiveTextEditor = (callback: option<TextEditor.t> => unit) =>
    vscode.window->onDidChangeActiveTextEditor(callback)
}

module Workspace = {
  @send external onDidChangeTextDocument_: (workspace, textDocumentChangeEvent => unit) => unit = "onDidChangeTextDocument"

  let onDidChangeTextDocument = (listener: textDocumentChangeEvent => unit) =>
    vscode.workspace->onDidChangeTextDocument_(listener)
}

module Position = {
  type t = position

  @module("vscode") @new
  external make: (~line: int, ~character: int) => t = "Position"
  @get external character: t => int = "character"
  @get external line: t => int = "line"

  let fromKakoune = (coord: KakouneTypes.Coord.t) => make(~line=coord.line, ~character=coord.column)

  let advance = (t, s: string) => {
    let lineFragments = s->Js.String2.split("\n")
    let newLine = t.line + Js.Array2.length(lineFragments) - 1
    let newCharacter = if newLine == t.line {
      t.character + Js.String2.length(s)
    } else {
      Js.String2.length(lineFragments[Js.Array2.length(lineFragments) - 1])
    }
    make(~line=newLine, ~character=newCharacter)
  }

  let start = make(~line=0, ~character=0)
}

module Range = {
  type t = range

  @module("vscode") @new
  external make: (~start: position, ~end: position) => range = "Range"

  let fromPosition = (t, s: string) =>
    make(~start=t, ~end=t->Position.advance(s))
}

module Selection = {
  type t = selection

  @module("vscode") @new
  external make: (~anchor: position, ~active: position) => t = "Selection"
  @get external anchor: t => position = "anchor"
  @get external active: t => position = "active"
  @get external start: t => position = "start"
  @get external end_: t => position = "end"

  let fromKakoune = selection =>
    if KakouneTypes.Selection.isBackward(selection) {
      make(
        ~anchor=selection.anchor->KakouneTypes.Coord.successor->Position.fromKakoune,
        ~active=selection.cursor->Position.fromKakoune)
    } else {
      make(
        ~anchor=selection.anchor->Position.fromKakoune,
        ~active=selection.cursor->Position.fromKakoune)
    }

  let asRange = selection => Range.make(~start=selection.anchor, ~end=selection.active)
}

let createChannel = (context) => {
  let channel = Window.createOutputChannel("Kakoune mode")
  Js.Array.push(channel, context.subscriptions)->ignore
  channel
}

let overrideCommand = (context, command, callback) =>
  Commands.registerCommand(command, args =>
    switch TextEditor.document() {
    | None => ()
    | Some(document) =>
      if ["file", "untitled"]->Js.Array2.includes(document.uri.scheme) {
        callback(args)
      } else {
        Commands.executeCommandWithArg("default:" ++ command, args)
      }
    }
  )
  ->Js.Array.push(context.subscriptions)
  ->ignore

let overrideTypeCommand = (context, writeKeys) =>
  overrideCommand(context, "type", args => {
    switch args.text {
    | Some(t) => t->writeKeys
    | None => ()
    }
  })

let registerWindowChangeEventHandler = writeKeys =>
  Window.onDidChangeActiveTextEditor(event =>
    switch event {
    | None => ()
    | Some(e) => (":e " ++ e.document.fileName ++ "<ret>")->writeKeys
    }
  )

let setCursorStyle = style =>
  switch TextEditor.options() {
  | None => ()
  | Some(o) => o.cursorStyle = style->TextEditor.cursorStyleToJs
  }

let updateCursorStyle = (newMode: Mode.t) =>
  switch newMode {
  | Mode.Normal => setCursorStyle(TextEditor.Block)
  | Mode.Insert => setCursorStyle(TextEditor.Line)
  | Mode.Unknown
  | Mode.EnterKey
  | Mode.EnterParam
  | Mode.Prompt => setCursorStyle(TextEditor.BlockOutline)
  }

let setSelection = selection => {
  TextEditor.activeTextEditor()
  ->Belt.Option.forEach(ed => {
    ed->TextEditor.setSelection(selection)
    ed->TextEditor.revealRange(selection->Selection.asRange)
  })
}

let setSelections = selections => {
  switch TextEditor.activeTextEditor() {
  | Some(ed) =>
    ed->TextEditor.setSelections(selections)
    ed->TextEditor.revealRange(selections[0]->Selection.asRange)
  | None => ()
  }
}

let createEditOps = (before, after): array<(TextEditor.editBuilder) => unit> => {
  let createOp = (accu, op: FastDiff.op) => {
    let (ops, pos) = accu
    switch op.opType {
    | Equal => (ops, pos->Position.advance(op.s))
    | Insert =>
      Js.log3("add INSERT", pos, op.s)
      ops
        ->Js.Array2.push(
          textEditorEdit => textEditorEdit->TextEditor.insert(pos, op.s))
        ->ignore
      (ops, pos->Position.advance(op.s))
    | Delete =>
      let deleteRange = Range.fromPosition(pos, op.s)
      Js.log2("add DELETE range", deleteRange)
      ops
        ->Js.Array2.push(
          textEditorEdit => textEditorEdit->TextEditor.delete(deleteRange))
        ->ignore
      (ops, pos->Position.advance(op.s))
    }
  }
  let (ops, _) = FastDiff.diff(before, after)->Js.Array2.reduce(createOp, ([], Position.start))
  ops
}

let replaceAll = text => {
  TextEditor.activeTextEditor()->Belt.Option.map(ed => {
    let rec runEdit = (attempts) => {
      let before = ed.document->TextDocument.getText
      let ops = createEditOps(before, text)
      let cb = textEditorEdit => ops->Js.Array2.forEach(f => f(textEditorEdit))
      if Js.Array2.length(ops) == 0 {
        Promise.resolve(0)
      } else {
        ed
        ->TextEditor.edit(cb, {
          undoStopBefore: false,
          undoStopAfter: false,
        })
        ->Promise.then(success => {
           if !success {
            runEdit(attempts + 1)
          } else {
            Promise.resolve(attempts)
          }
        })
      }
    }
    runEdit(1)->PromiseUtil.timeWithResult->Promise.then(((attempts, elapsed)) => {
        Js.log(`runEdit() completed after ${Js.Int.toString(attempts)} attempt${attempts == 1 ? "" : "s"} (${Js.Float.toFixedWithPrecision(elapsed *. 1000., ~digits=1)} ms)`)
      Promise.resolve()
    })
  })
  ->Belt.Option.getWithDefault(Promise.resolve())
}

let activePrompt: ref<option<QuickPick.t>> = ref(None)

let hidePrompt = () => {
  activePrompt.contents->Belt.Option.forEach(prompt => {
    activePrompt.contents = None
    prompt->QuickPick.dispose
  })
}

let showEnterKeyPrompt = (title, options, writeKeys) => {
  open QuickPick
  let prompt = make()
  prompt->setPlaceholder(title)
  prompt->setItems(Js.Array2.map(
    options,
    ((key, description)) => { "label": key, "description": description }))

  prompt->onDidAccept(() => {
    let activeItems = prompt.activeItems
    if Js.Array2.length(activeItems) > 0 {
      hidePrompt()
      writeKeys(activeItems[0]["label"]->Js.String2.substring(~from=0, ~to_=1))
    }
  })

  if Mode.getMode() == Mode.EnterKey {
    prompt->onDidChangeValue(key => {
      writeKeys(key)
      hidePrompt()
    })->ignore
  }

  prompt->onDidHide(() => {
    if Belt.Option.isSome(activePrompt.contents) {
      activePrompt.contents = None
      writeKeys("<esc>")
    }
  })->ignore

  hidePrompt()
  activePrompt.contents = Some(prompt)
  prompt->show
}

let showPrompt = (title, value, writeKeys) => {
  open Belt.Option
  open QuickPick
  if isNone(activePrompt.contents) {
    let prompt = make()
    activePrompt.contents = Some(prompt)
    prompt->onDidAccept(() => {
      hidePrompt()
      writeKeys("<ret>")
    })
    prompt->onDidChangeValue(newValue => writeKeys(
      "<c-u>" ++ Js.String2.replaceByRe(newValue, %re("/</g"), "<lt>")))
    prompt->onDidHide(() => {
      if isSome(activePrompt.contents) {
        activePrompt.contents = None
        writeKeys("<esc>")
      }
    })
    prompt->show
  }
  let prompt = getExn(activePrompt.contents)
  if prompt.title != title {
    prompt->setTitle(title)
  }
  if prompt.value != value {
    prompt->setValue(value)
  }
}