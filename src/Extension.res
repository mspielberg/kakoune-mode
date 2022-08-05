let activate = context => {
  switch Vscode.TextEditor.document() {
  | None => Node.spawn("kak", ["-ui", "json", "-s", "vscode"])->Kakoune.setKak
  | Some(doc) => Node.spawn("kak", ["-ui", "json", "-s", "vscode", doc.fileName])->Kakoune.setKak
  }

  Kakoune.getKak().stderr.on(. "data", Kakoune.handleIncomingError)
  Kakoune.getKak().stdout.on(. "data", data => Kakoune.handleIncomingCommand(data)->ignore)
  Kakoune.writeToKak->Kakoune.initKak

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
