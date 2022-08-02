type disposable
type position
type selection

type range = {
  start: position,
  end: position,
}

type uri = {
  scheme: string,
  toString: (. unit) => string,
}

type textDocument = {
  uri: uri,
  fileName: string,
}

type textCommandArgs = {
  text: option<string>
}

type textEditorOptions = {
  mutable cursorStyle: int
}

type textEditor = {
  document: textDocument,
  options: textEditorOptions,
  selection: selection,
}

type extension_context = {
  subscriptions: array<disposable>
}

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

type commands

type window = {
  activeTextEditor: option<textEditor>
}

type workspace

type vscode = {
  commands: commands,
  window: window,
  workspace: workspace,
}