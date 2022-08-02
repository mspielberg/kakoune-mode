open VscodeTypes

module Commands = {
  let registerCommand: (string, 'a => unit) => disposable = (name, callback) =>
    vscode["commands"]["registerCommand"](. name, callback)

  let executeCommand: string => unit = command => vscode["commands"]["executeCommand"](. command)

  let executeCommandWithArg: (string, textCommandArgs) => unit = (command, arg) =>
    vscode["commands"]["executeCommand"](. command, arg)
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
    | Some(t) => t->Rpc.KeysMessage.make->writeToKak
    | None => ()
    }
  })

let registerWindowChangeEventHandler = writeToKak =>
  Window.onDidChangeActiveTextEditor(event =>
    switch event {
    | None => ()
    | Some(e) => (":e " ++ e.document.fileName ++ "<ret>")->Rpc.KeysMessage.make->writeToKak
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
  | Mode.Prompt => setCursorStyle(TextEditor.BlockOutline)
  }

let setSelection = selection => {
  TextEditor.activeTextEditor()
  ->Belt.Option.forEach(ed => ed->TextEditor.setSelection(selection))
}

let setSelections = selections => {
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

let activePrompt: ref<option<VscodeTypes.QuickPick.t>> = ref(None)

let hidePrompt = () => {
  activePrompt.contents->Belt.Option.forEach(prompt => {
    activePrompt.contents = None
    prompt->QuickPick.dispose
  })
}

let showEnterKeyPrompt = (title, options, writeKeys) => {
  open VscodeTypes.QuickPick
  let prompt = make(.)
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
  open VscodeTypes.QuickPick
  if activePrompt.contents->Belt.Option.isNone {
    let prompt = make(.)
    activePrompt.contents = Some(prompt)
    prompt->QuickPick.onDidAccept(() => {
      hidePrompt()
      writeKeys("<ret>")
    })
    prompt->onDidChangeValue(newValue => writeKeys("<c-u>" ++ newValue))
    prompt->onDidHide(() => {
      if Belt.Option.isSome(activePrompt.contents) {
        activePrompt.contents = None
        writeKeys("<esc>")
      }
    })
    prompt->show
  }
  let prompt = Belt.Option.getExn(activePrompt.contents)
  if prompt.title != title {
    prompt->setTitle(title)
  }
  if prompt.value != value {
    prompt->setValue(value)
  }
}