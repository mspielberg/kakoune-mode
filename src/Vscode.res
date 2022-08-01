@module external vscode: {..} = "vscode"

type disposable

type position
type selection

type range = {
  start: position,
  @as("end")
  end_: position,
}

type uri = {
  scheme: string,
  toString: (. unit) => string,
}
type textDocument = {
  uri: uri,
  fileName: string,
}
type textCommandArgs = {text: option<string>}
type textEditorOptions = {mutable cursorStyle: int}
type textEditor = {
  document: textDocument,
  options: textEditorOptions,
  selection: selection,
}

type extension_context = {subscriptions: array<disposable>}

type textDocumentContentChangeEvent = {
  range: range,
  rangeLength: int,
  rangeOffset: int,
  text: string,
}
type textDocumentChangeEvent = {
  contentChanges: array<textDocumentContentChangeEvent>,
  document: textDocument,
}

type outputChannel
module OutputChannel = {
  @send external append: (outputChannel, string) => unit = "append"
  @send external appendLine: (outputChannel, string) => unit = "appendLine"
}

module Commands = {
  let registerCommand: (string, 'a => unit) => disposable = (name, callback) =>
    vscode["commands"]["registerCommand"](. name, callback)

  let executeCommand: string => unit = command => vscode["commands"]["executeCommand"](. command)

  let executeCommandWithArg: (string, textCommandArgs) => unit = (command, arg) =>
    vscode["commands"]["executeCommand"](. command, arg)
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

  let activeTextEditor: unit => option<textEditor> = () =>
    Js.toOption(vscode["window"]["activeTextEditor"])

  let options: unit => option<textEditorOptions> = () =>
    activeTextEditor()->Belt.Option.map(t => t.options)

  let document: unit => option<textDocument> = () =>
    activeTextEditor()->Belt.Option.map(t => t.document)

  @set external setSelection: (t, selection) => unit = "selection"
  @set external setSelections: (t, array<selection>) => unit = "selections"
  @send external edit: (t, editBuilder => unit, editOptions) => unit = "edit"

  @send external replace: (editBuilder, selection, string) => unit = "replace"
}

module Window = {
  type event<'a> = option<'a>

  let showError: string => unit = message => vscode["window"]["showErrorMessage"](. message)

  let onDidChangeActiveTextEditor: (event<TextEditor.t> => unit) => unit = event =>
    vscode["window"]["onDidChangeActiveTextEditor"](. event)
}

module Workspace = {
  let onDidChangeTextDocument: (textDocumentChangeEvent => unit) => unit = event =>
    vscode["workspace"]["onDidChangeTextDocument"](. event)
}

module Position = {
  type t = position

  @module("vscode") @new
  external make: (~line: int, ~character: int) => t = "Position"
  @get external character: t => int = "character"
  @get external line: t => int = "line"
}

module Selection = {
  type t = selection

  @module("vscode") @new
  external make: (~anchor: position, ~active: position) => t = "Selection"
  @get external anchor: t => position = "anchor"
  @get external active: t => position = "active"
  @get external start: t => position = "start"
  @get external end_: t => position = "end"
}

let createChannel = (context) => {
  let channel = vscode["window"]["createOutputChannel"](. "Kakoune mode")
  Js.Array.push(channel, context.subscriptions)->ignore
  channel
}

let overrideCommand = (context, command, callback) =>
  Commands.registerCommand(command, args =>
    switch TextEditor.document() {
    | None => ()
    | Some(document) =>
      switch (document.uri.toString(.), Mode.getMode()) {
      | ("debug:input", _currentMode) => Commands.executeCommandWithArg("default:" ++ command, args)
      | (_documentUri, Mode.Insert) =>
        Commands.executeCommandWithArg("default:" ++ command, args)
        callback(args)
      | _ => callback(args)
      }
    }
  )
  ->Js.Array.push(context.subscriptions)
  ->ignore

let overrideTypeCommand = (context, writeToKak) =>
  overrideCommand(context, "type", args => {
    switch args.text {
    | Some(t) => t->Rpc.createKeysMessage->Rpc.stringifyMessage->writeToKak
    | None => ()
    }
  })

let registerWindowChangeEventHandler = writeToKak =>
  Window.onDidChangeActiveTextEditor(event =>
    switch event {
    | None => ()
    | Some(e) =>
      Rpc.createKeysMessage(":e " ++ (e.document.fileName ++ "<ret>"))
      ->Rpc.stringifyMessage
      ->writeToKak
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
  | Mode.EnterKey => setCursorStyle(TextEditor.BlockOutline)
  }

let setSelection = selection => {
  Js.log2("setSelection", selection)
  TextEditor.activeTextEditor()
  ->Belt.Option.forEach(ed => ed->TextEditor.setSelection(selection))
}

let setSelections = selections => {
  Js.log2("setSelections", selections)
  switch TextEditor.activeTextEditor() {
  | Some(ed) => ed->TextEditor.setSelections(selections)
  | None => ()
  }
}

let replaceLine = (line, lineNumber) => {
  TextEditor.activeTextEditor()->Belt.Option.forEach(ed => {
    let selection = Selection.make(
      ~anchor=Position.make(~line=lineNumber, ~character=0),
      ~active=Position.make(~line=lineNumber + 1, ~character=0))
    let cb = textEditorEdit => textEditorEdit->TextEditor.replace(selection, line)
    ed->TextEditor.edit(cb, {
      undoStopBefore: false,
      undoStopAfter: false,
    })
  })
}

let replaceAll = text => {
  TextEditor.activeTextEditor()->Belt.Option.forEach(ed => {
    let selection = Selection.make(
      ~anchor=Position.make(~line=0, ~character=0),
      ~active=Position.make(~line=Js.Int.max, ~character=0))
    let cb = textEditorEdit =>
      textEditorEdit->TextEditor.replace(
        selection,
        text->Js.String2.substring(~from=0, ~to_=Js.String2.length(text) - 1))
    ed->TextEditor.edit(cb, {
      undoStopBefore: false,
      undoStopAfter: false,
    })
  })
}