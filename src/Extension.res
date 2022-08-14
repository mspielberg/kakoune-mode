let activate = context => {
  let filename = Vscode.TextEditor.document()
    ->Belt.Option.map(d => d.fileName)
  Kakoune.initKak(filename)

  Vscode.overrideTypeCommand(context, text => Kakoune.writeKeys(text))
  Vscode.Commands.registerCommand("extension.send_escape", () => Kakoune.writeKeys("<esc>"))
  ->Js.Array.push(context.subscriptions)
  ->ignore

  Vscode.Commands.registerCommand("extension.send_backspace", () => {
    switch Mode.getMode() {
    | Mode.Insert => Vscode.Commands.executeCommand("deleteLeft")
    | _ => ()
    }
    Kakoune.writeKeys("<backspace>")
  })
  ->Js.Array.push(context.subscriptions)
  ->ignore

  Vscode.registerWindowChangeEventHandler(Kakoune.writeKeys)
  Keybinds.registerAllKeys(context, Kakoune.writeKeys)
  Js.log("activate completed")
}
