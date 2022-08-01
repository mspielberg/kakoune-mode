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

module Prompt = {
  type quickPickItem = {
    "label": string,
    "description": string,
  }

  type t = {
    title: string,
    items: array<quickPickItem>,
    dispose: (. unit) => unit,
    show: (. unit) => unit,
    onDidChangeValue: (. string => unit) => disposable,
  }

  let make: (. unit) => t = (.) => vscode["window"]["createQuickPick"](.)

  @set external setItems: (t, array<quickPickItem>) => unit = "items"
  @set external setPlaceholder: (t, string) => unit = "placeholder"

  let show = qp => qp.show(.)
  
  let dispose = qp => qp.dispose(.)

  let onDidChangeValue = (qp, f) => qp.onDidChangeValue(. f)
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
